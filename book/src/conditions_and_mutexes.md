# Conditions & Mutexes

When it came to implementing our small scheduler and interacting with the
system, the main challenge was to address the issue of suspending a function so
that it could run in the background. However, it's not just syscalls that can
suspend/block the execution of a function. There are also Mutexes and
Conditions.

The real challenge of a scheduler is to be able to suspend functions without
involving the system: in other words, to manage all suspensions. For novices,
Mutexes and Conditions allow you to block and unblock the execution of a
function (possibly based on a predicate).

The usefulness of such mechanisms lies in synchronizing tasks with each other.
Whether they are in concurrency and/or in parallel, it is difficult, if not
impossible, to know which task will execute before the others. However, we
sometimes (and often) want to share information between these tasks. Miou only
allows one type of information transfer between tasks: from children to their
direct parents.

In all other cases (for example, between two tasks with no direct parent-child
relationship and executing in parallel), we need to consider how to transfer
this information correctly (meaning that this transfer would work regardless of
the execution order of our tasks from both Miou's perspective — for
`Miou.call_cc` — and the system's perspective — for `Miou.call`). It is in these
cases that Mutexes and Conditions can be useful.

## Mutexes

Mutexes allow obtaining exclusive access to manipulate information compared to
other tasks. This means that we can manipulate a global resource, available to
all tasks, securely using mutexes. To illustrate this example, let's revisit our
`echo` server where we want to display incoming connections as logs:
```ocaml
let pp_sockaddr ppf = function
  | Unix.ADDR_UNIX v -> Format.pp_print_string ppf v
  | Unix.ADDR_INET (inet_addr, port) ->
    Format.fprintf ppf "%s:%d" (Unix.string_of_inet_addr inet_addr) port

let server () =
  let socket = Miou_unix.Ownership.tcpv4 () in
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, 3000) in
  Miou_unix.Ownership.bind_and_listen socket sockaddr;
  let orphans = Miou.orphans () in
  while true do
    clean_up orphans;
    let client, sockaddr = Miou_unix.Ownership.accept socket in
    Format.printf "new client: %a\n%!" pp_sockaddr sockaddr;
    ignore (Miou.call_cc
      ~give:[ Miou_unix.Ownership.resource clientr 
      ~orphans (fun () -> echo client))
  done;
  Miou_unix.Ownership.close socket
```

It **may** happen (and this is the difficulty of parallel programming) that an
exception occurs seemingly out of nowhere if you run this code:
```ocaml
Fatal error: exception Stdlib.Queue.Empty
```

The real issue is that the `Format` module uses an internal queue to properly
indent your output (especially according to the [boxes][box]). In our case, this
queue ends up being manipulated by all our domains, and, as mentioned in the
[Stdlib.Queue][queue] documentation, the module is **not** thread-safe: the
documentation explicitly mentions the use of a mutex[^queue].

So, we need to protect our output between domains. To do this, a simple mutex is
necessary:
```ocaml
let mutex_out = Miou.Mutex.create ()

let printf fmt =
  let finally () = Miou.Mutex.unlock mutex_out in
  Miou.Mutex.lock mutex_out;
  Fun.protect ~finally @@ fun () ->
  Format.printf fmt
```

This way, we ensure that only one task executes our `Format.printf` and that the
others must wait for the first one to finish. We say it has exclusive access to
the resource.

## Conditions

A major issue with our `echo` server is its termination. Currently, we are
unable to terminate our server properly due to the infinite loop. However, we
could handle a system signal that instructs all our domains to terminate
gracefully. Since our main loop only accepts connections, we could implement a
function `accept_or_die` that, upon receiving a signal such as `SIGINT`,
initiates the process to terminate our domains.

Once again, a global resource comes into play — the signal sent by the system.
We need to return a `` `Die`` value instead of waiting for a new connection. The
purpose of a condition is to wait until a predicate (obtained using a global
resource) becomes true. In the case of our `echo` server, if we receive a
`SIGINT` signal, we return `` `Die``; otherwise, we continue waiting for a new
connection.
```ocaml
let condition = Miou.Condition.create ()
let mutex_sigint = Miou.Mutex.create ()

let accept_or_die fd =
  let accept () = `Accept (Miou_unix.Ownership.accept fd) in
  let or_die () =
    Miou.Mutex.protect mutex_sigint @@ fun () ->
    Miou.Condition.wait condition mutex_sigint;
    `Die in
  Miou.await_first [ Miou.call_cc accept; Miou.call_cc or_die ]
  |> function Ok value -> value | Error exn -> raise exn

let server () =
  let socket = Miou_unix.Ownership.tcpv4 () in
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, 3000) in
  Miou_unix.Ownership.bind_and_listen socket sockaddr;
  let rec go orphans =
    clean_up orphans;
    match accept_or_die socket with
    | `Die -> ()
    | `Accept (fd', sockaddr) ->
      printf "new client: %a\n%!" pp_sockaddr sockaddr;
      ignore (Miou.call_cc
        ~give:[ Miou_unix.Ownership.resource client ]
        ~orphans (fun () -> echo client));
      go orphans in
  go (Miou.orphans ())
```

We then need to "catch" the `SIGINT` signal. Signals are special in that they
can execute a task outside of Miou. However, if these tasks have side effects,
they won't be managed. Thus, Miou offers a way to attach functions to signals
using `Miou.set_signal`:
```ocaml
let stop _signal =
  Miou.Mutex.protect mutex_sigint @@ fun () ->
  Miou.Condition.broadcast condition

let () = Miou_unix.run @@ fun () ->
  Miou.set_signal Sys.sigint (Sys.Signal_handle stop);
  let domains = Stdlib.Domain.recommended_domain_count () - 1 in
  let domains = List.init domains (Fun.const ()) in
  let prm = Miou.call_cc server in
  Miou.await prm :: Miou.parallel server domains
  |> List.iter @@ function
  | Ok () -> ()
  | Error exn -> raise exn
```

This simply signals all places where our condition is waiting. Consequently, all
our domains are signaled to return `` `Die`` instead of continuing to wait for a
new connection.

### Ownership, sub-tasks and finalisers

If we try this code, it may not work, and Miou might complain with the
`Not_owner` exception. This is because our `accept` task does not own the
file-descritptor; we need to pass it the resource via the `give` parameter.

It's worth noting that this ownership is exclusive. Once we've performed
`Miou_unix.Ownership.accept`, we need to:
1) transfer the file-descritptor back to the parent (so it can transfer it to
   the next `accept`).
2) transfer the new file-descriptor to the parent that was created in our
   `accept` task so that it can transfer it to our `echo` task.

The importance of finalizers in this situation should also be noted. Indeed,
`await_first` will wait for one of the two tasks. If our condition unblocks and
returns `` `Die``, `await_first` will then cancel our `accept` task: we then
finish it in an abnormal situation where our finalizers will be called on our
file-descriptors. In other words, except for the active clients, all our
resources have been properly released by Miou, and we no longer need to take
care of them during the termination of our program.

Finally, even after these minor fixes, Miou may still return
`Still_has_children`. Indeed, receiving a signal does not mean that we have
finished all our children (we just cleaned up a few). However, we do know that:
- we will not have any new children.
- our `echo` task should terminate smoothly despite our signal.

So we need to `await` all our remaining children:
```ocaml
let rec terminate orphans =
  match Miou.care orphans with
  | None -> ()
  | Some None -> Miou.yield (); terminate orphans
  | Some (Some prm) ->
    match Miou.await prm with
    | Ok () -> ()
    | Error exn -> raise exn
```

## The final version of `echo`

If we take all our previous comments into account, here is the final version of
our `echo` server:
```ocaml
let condition = Miou.Condition.create ()
let mutex_sigint = Miou.Mutex.create ()
let mutex_out = Miou.Mutex.create ()

let printf fmt =
  let finally () = Miou.Mutex.unlock mutex_out in
  Miou.Mutex.lock mutex_out;
  Fun.protect ~finally @@ fun () ->
  Format.printf fmt

let rec echo client =
  let buf = Bytes.create 0x100 in
  let len = Miou_unix.Ownership.read client buf 0 (Bytes.length buf) in
  if len = 0 then Miou_unix.Ownership.close client
  else
    let str = Bytes.sub_string buf 0 len in
    let _ = Miou_unix.Ownership.write client str 0 len in echo client

let accept_or_die fd =
  let accept () =
    let fd', sockaddr = Miou_unix.Ownership.accept fd in
    Miou.Ownership.transfer (Miou_unix.Ownership.resource fd');
    Miou.Ownership.transfer (Miou_unix.Ownership.resource fd);
    `Accept (fd', sockaddr) in
  let or_die () =
    Miou.Mutex.protect mutex_sigint @@ fun () ->
    Miou.Condition.wait condition mutex_sigint;
    `Die in
  let give = [ Miou_unix.Ownership.resource fd ] in
  Miou.await_first [ Miou.call_cc ~give accept; Miou.call_cc or_die ]
  |> function Ok value -> value | Error exn -> raise exn

let pp_sockaddr ppf = function
  | Unix.ADDR_UNIX v -> Format.pp_print_string ppf v
  | Unix.ADDR_INET (inet_addr, port) ->
    Format.fprintf ppf "%s:%d" (Unix.string_of_inet_addr inet_addr) port

let clean_up orphans = match Miou.care orphans with
  | None | Some None -> ()
  | Some (Some prm) -> match Miou.await prm with
    | Ok () -> ()
    | Error exn -> raise exn

let rec terminate orphans =
  match Miou.care orphans with
  | None -> ()
  | Some None -> Miou.yield (); terminate orphans
  | Some (Some prm) ->
    match Miou.await prm with
    | Ok () -> ()
    | Error exn -> raise exn

let server () =
  let socket = Miou_unix.Ownership.tcpv4 () in
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, 3000) in
  Miou_unix.Ownership.bind_and_listen socket sockaddr;
  let rec go orphans =
    clean_up orphans;
    match accept_or_die socket with
    | `Die -> terminate orphans
    | `Accept (client, sockaddr) ->
      printf "new client: %a\n%!" pp_sockaddr sockaddr;
      ignore (Miou.call_cc
        ~give:[ Miou_unix.Ownership.resource client ]
        ~orphans (fun () -> echo client));
      go orphans in
  go (Miou.orphans ())

let stop _signal =
  Miou.Mutex.protect mutex_sigint @@ fun () ->
  Miou.Condition.broadcast condition

let () = Miou_unix.run @@ fun () ->
  Miou.set_signal Sys.sigint (Sys.Signal_handle stop);
  let domains = Stdlib.Domain.recommended_domain_count () - 1 in
  let domains = List.init domains (Fun.const ()) in
  let prm = Miou.call_cc server in
  Miou.await prm :: Miou.parallel server domains
  |> List.iter @@ function
  | Ok () -> ()
  | Error exn -> raise exn
```

You can compile it directly with `ocamlfind`, run it and, above all, test its
load with `parallel`:
```shell
$ ocamlfind opt -linkpkg -package miou,miou.unix main.ml
$ ./a.out &
$ cat >echo.sh<<EOF
#!/bin/bash

send() {
  echo "Hello World" | netcat -q0 localhost 3000
}

export -f send

while true; do
  parallel send ::: $(seq 100)
done
EOF
$ chmod +x echo.sh
$ ./echo.sh
```

Our final command launches a myriad of clients, with 100 of them executing
simultaneously. We can observe that all our domains are at work, and there are
no conflicts on the console thanks to our mutex. Finally, to appreciate all our
work, a `SIGINT` (with Ctrl+C) will terminate our server correctly and release
all our file descriptors!

This little project broadly demonstrates what is possible with Miou and the
insights that emerged during its development, particularly regarding system
resources and I/O. We hope this tutorial has sparked your interest in using Miou
in your applications. For the more adventurous, you can read our manifesto,
which explains, in a more social than technical manner, the benefits of Miou.

[^queue]: It is worth noting that Miou offers a thread-safe queue: `Miou.Queue`.
We use it internally for various purposes, particularly in inter-domain
synchronization. However, it is essential to recognize that `Miou.Queue` may
reveal other issues such as inter-domain contention.

[box]: https://v2.ocaml.org/api/Format.html#boxes
[queue]: https://v2.ocaml.org/api/Queue.html
