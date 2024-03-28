# A practical counter-example: a synchronous server

The benefits of asynchronous programming aren't immediately apparent to
everyone, and its understanding can be equally challenging. Therefore, we've
chosen to illustrate asynchronous programming with Miou through a counterexample
demonstrating its value: the implementation of a synchronous server. Our goal is
to then transform our synchronous server into an asynchronous one capable of
handling billions of connections simultaneously.

This tutorial presupposes that the reader is proficient in OCaml. While we aim
to provide comprehensive explanations of each step and the data manipulation
involved, we won't delve into basic OCaml concepts.

The goal of this tutorial is to implement an "echo" server. This server simply
echoes back whatever the user sends to it. While this may seem straightforward,
several challenges arise, including the issue of synchronicity.

## Socket

To facilitate communication between a client and a server, we utilize sockets.
These are fundamental components provided by the system for handling
communication, and in OCaml, we can manipulate them effectively. Let's delve
deeper into their functionality.

A socket acts as an endpoint for communication, enabling two computers to
connect and exchange data. It follows a _client-server_ model, where one side
initiates communication (the client), and the other side responds (the server).
In our example, we'll be focusing on implementing a server. To start, we need to
initialize a socket that's ready to accept connections.
```ocaml
val socket : socket_domain -> socket_type -> int -> file_descr
```

This function returns a file descriptor[^file-descriptor] representing the new
socket. Initially, this descriptor is "disconnected," meaning it's not yet set
up for reading or writing.

Several arguments are required, including the domain (determining whether the
socket communicates locally or over the Internet), the type of communication
(such as packet or stream communication), and the protocol used[^unix-tutorial].
For our purposes, establishing a TCP/IP connection suffices. Thus, we create our
socket as follows:
```ocaml
let server () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  ...
```

## Establish a service

After creating the socket, we need to assign a specific address to it so that it
can be reached from the network. This is done using the `bind` system call:
```ocaml
val bind : file_descr -> sockaddr -> unit
```

Our address must consist of an IP and a port since we intend our socket to
communicate over the Internet. In OCaml, the `sockaddr` value represents this
address. For our server, we want it to be available on our local network at port
3000:
```ocaml
let server () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, 3000) in
  Unix.bind socket sockaddr
```

Next, we specify that the socket can accept connections using the `listen`
system call:
```ocaml
val listen : file_descr -> int -> unit
```

The `listen` function requires our file descriptor to begin accepting client
connections. It also needs a second argument, specifying the maximum number of
pending incoming connections. Our server not only handles incoming connections
but also echoes back what clients transmit. It's possible that a client may
want to connect simultaneously, so the system keeps these clients on hold until
we can manage the new incoming connection.
```ocaml
let server () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, 3000) in
  Unix.bind socket sockaddr;
  Unix.listen socket 64
```

With everything initialized, we can now handle incoming connections using the
`accept` syscall:
```ocaml
val accept : file_descr -> file_descr * sockaddr
```

This function will **block** until a new connection arrives. It returns a new
file descriptor representing the client along with its address. To communicate
with the client, we use this new file descriptor. Now, let's complete the
implementation of our service.
```ocaml
let echo client =
  let buf = Bytes.create 0x100 in
  let len = Unix.read client buf 0 (Bytes.length buf) in
  if len = 0 then Unix.close client
  else let _ = Unix.write client buf 0 len in echo client

let server () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, 3000) in
  Unix.bind socket sockaddr;
  Unix.listen socket 64;
  let client, address_of_client = Unix.accept socket in
  echo client;
  Unix.close socket;
  print_endline "Server terminated"

let () = server ()
```

Now we're handling a client and calling our `echo` function, which reads what
the client sends and echoes it back until the client stops transmitting.

### Compilation & usage

Let's start testing our program! We need to compile it first before we can use
it. A tool like `netcat`/`nc` is sufficient to act as a client:
```shell
$ ocamlfind opt -linkpkg -package unix main.ml
$ ./a.out &
[1] 4347
$ echo "Hello World"|netcat -q0 localhost 3000
Hello World
Server terminated
[1]  + 4347 done       ./a.out
```

Our server worked well! It handled only one client, but it correctly echoed back
what `netcat` sent to it (as seen in echo "Hello World"). It's also noteworthy
that the server terminated correctly. However, at this point, there are still
several aspects to describe.

## A step back

There are several concepts we need to clarify in this exercise that are crucial
when it comes to implementing a system and network application.

### Synchronicity

While it may be obvious to some, it's important to clarify this concept to fully
understand the following steps. If we review our code, we can describe what it
does:
1) It creates a socket.
2) It defines an address.
3) It "binds" this socket to our address.
4) It instructs our system to make our socket available on the network.
5) It waits for a new connection.
6) It executes the `echo` function with our new incoming connection.
7) It displays "Server terminated".

It's important to note that your system processes the program line by line, in
the order we wrote it. At each step, the system waits for the current line to
finish its execution before moving on to the next one. This is necessary because
each line depends on the work performed in the preceding lines.

This characteristic makes our program **synchronous**. Even if we introduce the
`echo` function, the program remains synchronous because the caller must wait
for the function to complete its task and return a value before continuing.

### Blocking function

Previously, we introduced the `accept` function, which waits for a connection to
arrive. It's worth noting that if no connection arrives, your program will wait
indefinitely! We say the function **blocks**, meaning it's waiting for an
external event (like the arrival of a client). And we can't do anything else as
long as this function is blocked. This type of function makes it impossible for
a program to carry out other computations while waiting for the former to
finish.

The reason for this is that OCaml programs are single-threaded[^multicore]. A
thread is a sequence of instructions that a program follows. Because the program
consists of a single thread, it can only do one thing at a time: so if it is
waiting for our long-running synchronous call to return, it can't do anything
else.

## Handle multiple clients

Our goal now is to handle more than one client. We could simply repeat our
`accept` call every time a client arrives.
```ocaml
let echo client =
  let buf = Bytes.create 0x100 in
  let len = Unix.read client buf 0 (Bytes.length buf) in
  if len = 0 then Unix.close client
  else let _ = Unix.write client buf 0 len in echo client

let server () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, 3000) in
  Unix.bind socket sockaddr;
  Unix.listen socket 64;
  while true do
    let client, address_of_client = Unix.accept socket in
    echo client
  done;
  Unix.close socket;
  print_endline "Server terminated"

let () = server ()
```

Let's try our code:
```shell
$ ocamlfind opt -linkpkg -package unix main.ml
$ ./a.out &
[1] 8592
$ echo "Hello World"|netcat -q0 localhost 3000
Hello World
$ echo "Hello World"|netcat -q0 localhost 3000
Hello World
$ kill -9 8592
[1]  + 8592 killed     ./a.out
```

It seems to work! However, let's try a specific scenario: when two clients try
to connect "at the same time". First, we'll launch a background client that will
simply connect, and then we'll launch another client that will attempt to send
some text.
```shell
$ ./a.out &
[1] 8711
$ netcat localhost 3000 &
[2] 8728
[2]  + 8728 suspended (tty input)  netcat localhost 4000
$ echo "Hello World"|netcat -q0 localhost 4000
^C
$ kill -9 8728
$ kill -9 8711
```

Our second client, after our first one is connected, gets stuck. In reality,
when our first client connected, it made our server unavailable. This goes back
to our explanation of **synchronicity**: our program can strictly only do one
thing at a time. So, our program is currently handling our first client, and it
can't handle our second client until the first one is finished. In practice, our
`echo` function must finish so that our server can handle other clients.

## Asynchronicity

We're starting to see the fundamental problem of synchronicity in implementing a
system and network application: the ability for our service to respond to all
clients "at the same time." In our specific case, what we want is to be able to
background our echo function so that our server can wait for a new connection
again with accept. However, the concept of backgrounding a task is not so
straightforward:
* We know that we only have one _thread_ available, so we can strictly only do
  one thing. Which thread could execute our background task?
* We know that some functions put us in a waiting state (waiting for a new
  connection or waiting for data sent by the client). Instead of waiting, could
  we seize this opportunity to do "something else"?
* Ultimately, we primarily want to respond to events coming from the system.

Several solutions exist for this. They vary even more depending on the language
used and what it can offer to address these issues. Regarding OCaml 5, two
elements can help us:
- Effects
- Domains

For the next chapter, we'll focus on effects and follow our second intuition.
Namely, taking the opportunity to do something else as soon as we're waiting for
an event such as the arrival of a connection with accept.

[^file-descriptor]: It is a unique identifier used by your system to represent
an input/output resource. In concrete terms, it's a non-negative integer that
serves as a reference to I/O channel within a process.

[^unix-tutorial]: These parameters may seem daunting for a newcomer. If you're
interested in delving deeper into system programming with OCaml and Unix, we
recommend checking out this [tutorial][unix-tutorial].

[^multicore]: The more curious will say that OCaml is "multicore", and that's
true. However, it is if you want to use the `Domain`/`Thread` module and
allocate a domain/thread that can do a task in parallel or concurrently. But
we'll explain all this in detail later.

[unix-tutorial]: https://ocaml.github.io/ocamlunix/index.html
