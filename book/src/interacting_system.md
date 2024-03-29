# Interacting with the system

In the implementation of our `echo` server, beyond the question of
synchronicity, there were also considerations about interactions with the system
and the resources it provides, such as sockets.

We noticed that to manage these resources, we had functions described as
"blocking", meaning they waited for specific events (such as a new client
connection) before proceeding. Apart from wanting to delegate tasks in the
background, we also aimed to leverage these situations to perform other tasks.

We have this opportunity with `Await`, which observes the state of our promise
and then decides to continue if it contains the result of our task or to "yield"
(i.e., execute other tasks) if the associated task is not yet complete.

We could reproduce the same approach for these blocking functions: continue if
they have an event to notify us about, or "yield" if we know they will block.
The crucial question then is to predict in advance whether they will block.
Fortunately, the system can provide us with this information.

## File-descriptors

In our first chapter, we introduced the concept of file descriptors. These are
system resources used to manage I/O operations such as handling client
connections, transmitting bytes, and more. It's essential to monitor the state
of these resources and determine beforehand whether functions like `accept()`
(for managing a new client) will block or not.

Typically, we can consider that all our functions interacting with the system
block by default. However, we can periodically check our active file descriptors
to determine if we can safely resume functions that will perform these blocking
system calls.

Monitoring the state of our active file descriptors and determining if an event
(which would unblock our functions) occurs is done using the `select()`
function:
```ocaml
val select :
    file_descr list -> file_descr list -> file_descr list ->
      float -> file_descr list * file_descr list * file_descr list
```

This function takes several arguments, but only 3 are of interest to us. The
first and second arguments pertain to monitoring file descriptors that are
awaiting "read" and "write" operations, respectively. Typically, when we want to
wait for a client connection, we are waiting for a "read" operation on our file
descriptor. If we intend to transmit bytes to the client, we are waiting to be
able to "write" to our file descriptor. The last argument that concerns us is
the timeout for this observation. A reasonably short time is sufficient; let's
say 10ms.

For example, let's consider our accept() function. We want to determine whether
we should execute accept() without blocking:
```ocaml
let rec our_accept file_descr =
  print_endline "Monitor our file-descriptor.";
  match Unix.select [ file_descr ] [] [] 0.01 with
  | [], _, _ -> our_accept file_descr
  | file_descr :: _, _, _ -> Unix.accept file_descr

let server () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, 3000) in
  Unix.bind socket sockaddr;
  Unix.listen socket 64;
  let client, sockaddr = our_accept socket in
  Unix.close client;
  Unix.close socket
```

Let's test this code and see what happens.
```shell
$ ocamlfind opt -linkpkg -package unix main.ml
$ ./a.out &; sleep 1; netcat -q0 localhost 3000
Monitor our file-descriptor.
Monitor our file-descriptor.
Monitor our file-descriptor.
...
[1]  + 6210 done       ./a.out
```

Testing this code reveals the repeated "Monitoring our file-descriptor."
messages until the program ends (after receiving a connection using `netcat`
after 1 second). What's interesting here is that instead of blocking on
`accept()`, we execute it only if `select()` informs us that our file descriptor
is indeed ready (meaning it has received an incoming connection). If not, we
retry the observation by calling `select()` again.

In more concrete terms, we are no longer in a situation where we indefinitely
wait for an event to unblock us, but rather we wait for just 10ms to retry an
observation or execute our `accept()` if ready. We've found a way to determine
in advance whether our function will block or not.

## Integration into our scheduler

Now, the advantage of `select()` is that it can observe **multiple** file
descriptors (not just one as in our example). Our goal is to provide an
`our_accept` function that doesn't block. In case our file descriptor isn't
ready (which is the default case, as a reminder, all our system functions
block), we'll reuse our `Await` to suspend the execution before actually
performing our `accept()`. This suspension will give us the opportunity to
execute other tasks.
```ocaml
let waiting_fds_for_reading = Hashtbl.create 0x100

let our_accept file_descr =
  let value = ref None in
  Hashtbl.add waiting_fds_for_read file_descr value;
  Effect.perform (Await value);
  Hashtbl.remove waiting_fds_for_reading file_descr;
  Unix.accept file_descr
```

Finally, periodically, we'll observe all the file descriptors that are waiting.
`select()` will inform us about those that can be unblocked. We just need to
_fulfill_ our promise so that our scheduler can resume our suspended function.
```ocaml
let fullfill tbl fd =
  let value = Hashtbl.find tbl fd in
  value := Some ()

let our_select () =
  let rds = Seq.to_list (Hashtbl.to_seq_keys waiting_fds_for_reading) in
  let rds, _, _ = Unix.select rds [] [] 0.01 in
  List.iter (fullfill waiting_fds_for_reading) rds
```

Ultimately, we just need to call `our_select()` periodically. We previously
mentioned that our scheduler tries to resolve our tasks step by step. We'll
interleave these steps with this observation. This way, we'll be almost
immediately aware of the occurrence of events (within 10ms and a snippet of a
task execution).
```ocaml
let run fn =
  let result = ref None in
  let rec go = function
    | [] -> Option.get !result
    | Task task :: rest ->
        let todo = ref rest in
        let todo =
          match step todo task with
          | Resolved _ -> !todo
          | (Initial _ | Suspended _) as task -> !todo @ [ Task task ]
        in
        our_select (); go todo
  in
  let task = Initial (fun () -> result := Some (fn ())) in
  go [ Task task ]
```

## Let's try!

Let's revisit our example with `accept()`:
```ocaml
let server () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, 3000) in
  Unix.bind socket sockaddr;
  Unix.listen socket 64;
  let client, sockaddr = our_accept socket in
  Unix.close client;
  Unix.close socket

let () = run server
```

If we execute our server with our scheduler:
```shell
$ ocamlfind opt -linkpkg -package unix main.ml
$ ./a.out &; netcat -q0 localhost 3000
[1] 38255
[1]  + 38255 done       ./a.out
```

We notice that our program does not indefinitely block. It only blocks
periodically for 10ms[^busy-loop] and observes the active file descriptors (in
our `waiting_fds_for_reading` table). Finally, as soon as `netcat` connects, we
can resume our `our_accept` function and continue executing our program. With
the ability to put tasks in the background, we can now attempt to reimplement
our server asynchronously. However, we need to provide, just like `our_accept`,
`our_read` and `our_write`. The first one will reuse our
`waiting_fds_for_reading` table, while the second one will use a new table to
determine if our file descriptors are ready to transmit bytes.
```ocaml
let our_read file_descr buf off len =
  let value = ref None in
  Hashtbl.add waiting_fds_for_reading file_descr value;
  Effect.perform (Await value);
  Hashtbl.remove waiting_fds_for_reading file_descr;
  Unix.read file_descr buf off len

let waiting_fds_for_writing = Hashtbl.create 0x100

let our_write file_descr buf off len =
  let value = ref None in
  Hashtbl.add waiting_fds_for_writing file_descr value;
  Effect.perform (Await value);
  Hashtbl.remove waiting_fds_for_writing file_descr;
  Unix.write file_descr buf off len

let our_select () =
  let rds = Seq.to_list (Hashtbl.to_seq_keys waiting_fds_for_reading) in
  let wrs = Seq.to_list (Hashtbl.to_seq_keys waiting_fds_for_writing) in
  let rds, wrs, _ = Unix.select rds wrs [] [] 0.01 in
  List.iter (fullfill waiting_fds_for_reading) rds;
  List.iter (fullfill waiting_fds_for_reading) wrs
```

Now, we can both await new connections and manage in background our clients:
```ocaml
let rec echo client =
  let buf = Bytes.create 0x100 in
  let len = our_read client buf 0 (Bytes.length buf) in
  if len = 0 then Unix.close client
  else let _ = our_write client buf 0 len in echo client

let server () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, 3000) in
  Unix.bind socket sockaddr;
  Unix.listen socket 64;
  while true do
    let client, address_of_client = our_accept socket in
    ignore (spawn @@ fun () -> echo client)
  done;
  Unix.close socket;
  print_endline "Server terminated"

let () = run server
```

To test this code, simply launch your server and run 2 `netcat` instances
simultaneously (in 2 different terminals). You'll notice that our server no
longer blocks and can handle these 2 clients "simultaneously". We have finally
succeeded in creating an asynchronous server with effects in OCaml.
```shell
$ ocamlfind opt -linkpkg -package unix main.ml
$ ./a.out &; \
  echo "Salut"|netcat -q0 localhost 3000; \
  echo "Hello"|netcat -q0 localhost 3000
[1] 40381
Salut
Hello
$ kill -9 40381
[1]  + 40381 killed     ./a.out
```

[^busy-loop]: The purpose of the 10ms interval is to prevent our program from
falling into what is known as a ["busy-loop"][busy-loop]. Indeed, these 10ms
intervals notify our system that our program will do nothing during this time
period unless an event occurs. Our system is then able to put our program to the
_sleep_ mode and also take the opportunity to do something else. What is certain
is that this _sleep_ mode allows our program not to monopolize the processor. In
the case of a "busy-loop," our program would be the only one able to run, and
you would likely hear your processor fan whirring loudly.

[busy-loop]: https://en.wikipedia.org/wiki/Busy_waiting
