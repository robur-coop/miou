# Interacting with the system

In the implementation of our `echo` server, beyond the question of
synchronicity, there were also considerations about interactions with the system
and the resources it provides, such as sockets.

We also observed that to manipulate these resources, we had functions that we
described as "blocking" meaning they waited for an event to occur (such as the
connection of a new client) before unblocking. In addition to wanting to put
tasks in the background, we also wanted to take the opportunity to do something
else if we encountered such a function.

We have this opportunity with `Await`, which observes the state of our promise
and then decides to continue if it contains the result of our task or to "yield"
(meaning, execute other tasks) if the associated task is not yet complete.

We could reproduce the same approach for these blocking functions: continue if
they have an event to notify us about, or "yield" if we know they will block.
The crucial question then is to know in advance if they will block before
actually blocking. The system can provide us with such information.

## File-descriptors

In our first chapter, we introduced the concept of file descriptors. These are
system resources used to manage I/O operations such as handling client
connections, transmitting bytes, and more. It's essential to monitor the state
of these resources and determine beforehand whether functions like `accept()`
(for managing a new client) will block or not.

Essentially, we can consider that all our functions interacting with the system
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

Let's take an example with our `accept()` function. We would like to know if we
should indeed execute `accept()` without it blocking, using `select()`:
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
  Unix.close socket;
  Unix.close client
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

Here, we notice the appearance of multiple "Monitor our file-descriptor."
messages until our program terminates (as it just received a connection using
`netcat` after 1 second). What's interesting here is that instead of blocking on
`accept()`, we execute it only if `select()` informs us that our file descriptor
is indeed ready (meaning it has received an incoming connection). If not, we
retry the observation by calling `select()` again.

In more concrete terms, we are no longer in a situation where we indefinitely
wait for an event to unblock us, but rather we wait for just 10ms to retry an
observation or execute our `accept()` if ready. We've found a way to determine
in advance whether our function will block or not!

## Integration into our scheduler

Now, the advantage of `select()` is that it can observe **multiple** file
descriptors (not just one as in our example). Our goal is to provide an
`our_accept` function that doesn't block. In case our file descriptor isn't
ready (which is the default case, as a reminder, all our system functions
block), we'll reuse our `Await` to suspend the execution before actually
performing our `accept()`. This suspension will give us the opportunity to
execute other tasks.
```ocaml
let waiting_fds_for_read = Hashtbl.create 0x100

let our_accept file_descr =
  let value = ref None in
  Hashtbl.add waiting_fds_for_read file_descr value;
  Effect.perform (Await value);
  Unix.accept file_descr
```

Finally, periodically, we'll observe all the file descriptors that are waiting.
`select()` will inform us about those that can be unblocked. We just need to
_fulfill_ our promise so that our scheduler can resume our suspended function.
```ocaml
let fullfill fd =
  let value = Hashtbl.find waiting_fds_for_reading fd in
  value := Some ()

let our_select () =
  let rds = Seq.to_list (Hashtbl.to_seq_keys waiting_fds_for_reading) in
  let rds = Unix.select rds [] 0.01 in
  List.iter fullfill rds
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
        select (); go todo
  in
  let task = Initial (fun () -> result := Some (fn ())) in
  go [ Task task ]
```

## Let's try!
