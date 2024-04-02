# Interacting with the system with Miou

Miou is structured similarly to our small scheduler, but with a clear
distinction between scheduling and system interaction.

The goal is to be able to inject a certain type of interaction based on the
system used, which may not necessarily be `Unix`. As mentioned in the
introduction, our cooperative also aims to implement services as unikernels.
These are very specific systems where the concept of files may not even exist.
In this sense, `Miou_unix` is specific to `Unix`.

`Unix` and `Miou_unix` may suffice for most use cases. However, they may not be
suitable for unikernels and may not meet your criteria and system requirements.
A system like Linux may offer alternative means such as `epoll()` for
interacting with the external world, for example.

Beyond other possibilities, subtle differences may also exist. Indeed, systems
like Linux or \*BSD may not necessarily offer the same semantics in their
handling of TCP/IP connections, for instance. These nuances make it difficult
and even _error-prone_ to try to standardize all these interactions. Overall, we
believe that you are the best person to know how to interact with the system. As
such, we offer this small tutorial to guide you on how to implement the _glue_
between Miou and your system.

## Sleepers

One of the simplest interactions we can implement with Miou is the _sleeper_.
It's akin to what `Unix.sleep` does: waiting for a certain amount of time. We'll
iterate through its implementation to:
- Start by offering such a function.
- Handle the scenario where multiple domains might use this function.
- Finally, manage the cancellation of tasks that have called this function.

To guide our tutorial, here's the code we aim to execute. The module `Chat` is
the one we need to implement.
```ocaml
let program () =
  Chat.run @@ fun () ->
  let a = Miou.call_cc @@ fun () -> Chat.sleep 1. in
  let b = Miou.call_cc @@ fun () -> Chat.sleep 2. in
  Miou.await_all [ a; b ]
  |> List.iter @@ function
  | Ok () -> ()
  | Error exn -> raise exn

let () =
  let t0 = Unix.gettimeofday () in
  program ();
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 < 3.)
```

If we assume that tasks run "simultaneously," this program should execute in
less than 3 seconds.

## Our sleeper function

If you remember from our little scheduler, we reused our `Await` effect to
implement our `our_accept` function. We then injected our `our_select` into our
scheduler so that it could observe events and signal suspension points to resume
them.

Similarly, Miou offers such mechanisms. You can suspend a function using a
unique identifier called `Miou.syscall`:
```ocaml
type syscall
type uid = private int [@@immediate]

val syscall : unit -> syscall
val uid : syscall -> uid
val suspend : syscall -> unit
```

The equivalent of our `Effect.perform (Await prm)` from our little scheduler
would be `Miou.suspend`. Therefore, our main goal is to save our syscall
_somewhere_ and attach the time we need to wait to it:
```ocaml
type sleeper =
  { time : float
  ; syscall : Miou.syscall }

module Sleepers = Miou.Pqueue.Make (struct
  type t = sleeper

  let compare { time= a; _ } { time= b; _ } = Float.compare a b
  let dummy = { time= 0.0; syscall= Obj.magic () }
end)

let sleepers = Sleepers.create ()

let sleep delay =
  let time = Unix.gettimeofday () +. delay in
  let syscall = Miou.syscall () in
  Sleepers.insert { time; syscall } sleepers;
  Miou.suspend syscall
```

Using a priority queue[^proof] will allow us to get the syscall that needs to
resume its function as soon as possible among all our sleepers.

Now we need to find a way to inject a function that will be called periodically,
in which we can resume our functions just like we did for our little scheduler
with our `our_select` function. Fortunately, Miou offers this possibility of
injecting such a function via `Miou.run`:
```ocaml
type select = block:bool -> uid list -> signal list
type events = { select: select; interrupt: unit -> unit }

val run : ?events:(Domain.Uid.t -> events) -> (unit -> 'a) -> 'a
```


At the very beginning, Miou tries to allocate the domains. For each domain, it
will ask for an `event` value that contains a `select` function. This is the
function we need to implement and provide to Miou. This function will be called
whenever there is a syscall after executing a portion of our tasks and
periodically. This function must return signals allowing Miou to resume
functions that have been suspended by our syscalls.
```ocaml
type signal

val signal : syscall -> signal
```

So, one of its goals is to determine if one of our sleepers should resume one of
our tasks or not.
```ocaml
let in_the_past ts = ts = 0. || ts <= Unix.gettimeofday ()

let rec remove_and_signal_older sleepers acc =
  match Sleepers.find_min sleepers with
  | None -> acc
  | Some { time; syscall } when in_the_past time ->
    Sleepers.delete_min_exn sleepers;
    remove_and_signal_older sleepers (Miou.signal syscall :: acc)
  | Some _ -> acc

let select ~block:_ _ =
  let ts = match Sleepers.find_min sleepers with
    | None -> 0.0
    | Some { time; _ } ->
      let value = time -. Unix.gettimeofday () in
      Float.max 0.0 value in
  Unix.sleepf ts;
  remove_and_signal_older sleepers []

let events _ = { Miou.select; interrupt= ignore }
let run fn = Miou.run ~events fn
```

As you can see, we are specializing our `Miou.run` function with our `events`.
That's why whenever you use functions from `Miou_unix`, for example, you should
use `Miou_unix.run`. Let's now try our code:
```shell
$ ocamlfind opt -linkpkg -package miou,unix -c chat.ml
$ ocamlfind opt -linkpkg -package miou,unix chat.cmx main.ml
$ ./a.out
$ echo $?
0
```

And there you go! We've just proven that our two tasks are running concurrently.
Note the use of `Unix.sleepf` instead of `Unix.select`. Here, we are only
interested in waiting rather than observing our file descriptors.

## Domains & system

As mentioned earlier, Miou manages multiple domains. Therefore, if our `select`
function uses a global variable like `sleepers`, we will definitely encounter an
access problem with this variable across domains. There are several solutions to
this issue, one of which involves "protecting" our global variable using a
Mutex. However, we have specified that each domain manages its own `select`.

In general, a syscall is always local to a domain; it cannot be managed by
another domain that did not suspend it. In this sense, we can consider
allocating a `sleepers` for each domain. In OCaml, there is a way to consider
values that exist and are accessible to each domain, and these domains have
exclusivity over them: it's called [Thread Local Storage][tls].

So instead of having a global `sleepers`, we will use this API:
```ocaml
let sleepers =
  let key = Stdlib.Domain.DLS.new_key Sleepers.create in
  fun () -> Stdlib.Domain.DLS.get key

let sleep delay =
  let sleepers = sleepers () in
  let time = Unix.gettimeofday () +. delay in
  let syscall = Miou.syscall () in
  Sleepers.insert { time; syscall } sleepers;
  Miou.suspend syscall

let select ~block:_ _ =
  let sleepers = sleepers () in
  let ts = match Sleepers.find_min sleepers with
    | None -> 0.0
    | Some { time; _ } ->
      let value = time -. Unix.gettimeofday () in
      Float.max 0.0 value in
  Unix.sleepf ts;
  remove_and_signal_older sleepers []
```

We can now safely replace our `Miou.call_cc` with `Miou.call`. We know that each
task will have its own `sleepers`, and there will be no illegal access between
domains.

## Cancellation

There is one last point remaining: cancellation. Indeed, a task that has
suspended on a syscall can be canceled. We need to "clean up" our syscalls and
consider some of them unnecessary to observe. The `select` function has a final
argument corresponding to a list of canceled syscalls (with their unique
identifiers). When cancellation occurs, Miou attempts to collect all canceled
syscalls and then provides them to you so that you can clean up your variables
from these syscalls (in this case, our `sleepers`).

Another subtlety concerns inter-domain synchronization. Cancellation may require
synchronization between two domains, especially if we use `Miou.call` where the
promise exists in a different domain than the executing task. The problem is
that this synchronization may occur when one of the two domains performs
`Unix.sleepf`: in this case, we would need to wait for our domain to complete
its operation before continuing with cancellation. This is obviously not
feasible, especially if cancellation involves cleaning up a myriad of promises
and tasks across multiple domains (recall that `Miou.cancel` also cancels
children).

We have not mentioned it yet, but `events` has another function called
`interrupt`. This function precisely allows Miou to interrupt `select` because
the state of a promise has changed (and this change must be taken into account
in our syscall management).

So the question is: how do we interrupt `Unix.sleepf`? There are several
solutions, such as sending a signal, for example (and generating the
`EINTR`[^thread] exception). However, we could just as well reuse `Unix.select`.
To remind you, this function can wait for a certain time (just like
`Unix.sleepf`) but can also be interrupted as soon as an event occurs (such as
the execution of `interrupt` by Miou). The idea is to create a file descriptor
that Miou can manipulate and that `Unix.select` can observe.

Once again, the idea is not to interrupt the entire domain. An interruption does
not necessarily mean that we want to wake up the domain because we know it is in
a state where it cannot handle cancellation—actually, we cannot know that. The
interruption just aims to unblock the domain only if it is performing a
`select`. Thus, it may happen that Miou attempts to interrupt multiple times
when it is unnecessary — because the domain in question is not operating on the
`select`. But that's okay; we can simply ignore these interruptions.

So, we have two things to do to manage cancellation:
1) Clean up our sleepers from the syscalls given as arguments to our `select`
   function.
2) Be able to interrupt our `select` using a file descriptor that Miou could
   use.

Let's start by cleaning up our syscalls. The problem with `Miou.Pqueue` is that
we cannot arbitrarily delete an element unless it is the smallest element.
Indeed, we could cancel a sleeper that is not necessarily the next in terms of
time. But we could _tag_ our sleepers as canceled and simply ignore them when
we should signal them:
```ocaml
type sleeper =
  { time : float
  ; syscall : Miou.syscall
  ; mutable cancelled : bool }

module Sleepers = Miou.Pqueue.Make (struct
  type t = sleeper

  let compare { time= a; _ } { time= b; _ } = Float.compare a b
  let dummy = { time= 0.0; syscall= Obj.magic (); cancelled= false }
end)

let sleepers =
  let key = Stdlib.Domain.DLS.new_key Sleepers.create in
  fun () -> Stdlib.Domain.DLS.get key

let sleep delay =
  let sleepers = sleepers () in
  let time = Unix.gettimeofday () +. delay in
  let syscall = Miou.syscall () in
  Sleepers.insert { time; syscall; cancelled= false } sleepers;
  Miou.suspend syscall

let rec remove_and_signal_older sleepers acc =
  match Sleepers.find_min sleepers with
  | None -> acc
  | Some { cancelled; _ } when cancelled ->
    Sleepers.delete_min_exn sleepers;
    remove_and_signal_older sleepers acc
  | Some { time; syscall; _ } when in_the_past time ->
    Sleepers.delete_min_exn sleepers;
    remove_and_signal_older sleepers (Miou.signal syscall :: acc)
  | Some _ -> acc

let rec clean sleepers uids =
  let f ({ syscall; _ } as elt) =
    if List.exists ((=) (Miou.uid syscall)) uids
    then elt.cancelled <- true in
  Sleepers.iter f sleepers

let rec minimum sleepers =
  match Sleepers.find_min sleepers with
  | None -> None
  | Some { cancelled; _ } when cancelled ->
    Sleepers.delete_min_exn sleepers;
    minimum sleepers
  | Some elt -> Some elt

let select ~block:_ uids =
  let sleepers = sleepers () in
  clean sleepers uids;
  let ts = match minimum sleepers with
    | None -> 0.0
    | Some { time; _ } ->
      let value = time -. Unix.gettimeofday () in
      Float.max 0.0 value in
  Unix.sleepf ts;
  remove_and_signal_older sleepers []
```

Now, we need to implement our `interrupt` and modify our `select` accordingly so
that it can handle the interruption. As we explained, we will use `Unix.select`
to both wait for the necessary time (like `Unix.sleepf`) and observe a specific
event: whether we have been interrupted or not.

The interruption mechanism will be done using a file descriptor (because this is
what we can observe). We need to transmit a signal of some sort via our
`interrupt` function that `select` can handle. So we will create a _pipe_ where
`interrupt` writes to it, and `select` reads from it as soon as it has bytes
available:
```ocaml
let consume ic =
  let buf = Bytes.create 0x100 in
  ignore (Unix.read ic buf 0 (Bytes.length buf))

let select ic ~block:_ uids =
  let sleepers = sleepers () in
  clean sleepers uids;
  let ts = match minimum sleepers with
    | None -> 0.0
    | Some { time; _ } ->
      let value = time -. Unix.gettimeofday () in
      Float.max 0.0 value in
  match Unix.select [ ic ] [] [] ts with
  | [], _, _ -> remove_and_signal_older sleepers []
  | ic :: _, _, _ ->
    consume ic;
    remove_and_signal_older sleepers []

let buf = Bytes.make 1 '\000'

let events _ =
  let ic, oc = Unix.pipe () in
  let interrupt () = ignore (Unix.write oc buf 0 (Bytes.length buf)) in
  { Miou.select= select ic; interrupt }
```

And there you have it! We can now allow Miou to interrupt a `select` in the case
of cancellation. Interruption occurs quite frequently and is not limited to
cancellation. Here, we are mainly interested in unblocking our `Unix.select` so
that Miou can then handle its tasks immediately. It is also worth noting that we
don't consume our pipe interruption by interruption. There may be unnecessary
interruptions that we can ignore (hence reading 0x100 bytes instead of 1).

To demonstrate the validity of our implementation, we can try canceling a task
in parallel that would take too long:
```ocaml
let () = Chat.run @@ fun () ->
  let prm = Miou.call @@ fun () -> Chat.sleep 10. in
  Miou.yield ();
  Miou.cancel prm
```

This code should not take 10 seconds but just the time it takes for
cancellation.

## Blocking indefinitely

As you can imagine, we have still omitted some details in our tutorial.
Particularly, the `block` option. This option signals from Miou that there are
no tasks left to handle, and only syscalls can unblock your application. This is
typically what we expect from a system and network application: fundamentally
waiting for system events as a top priority.

In this regard, if `block:true`, we can afford to wait indefinitely (while still
considering possible interruptions) without handing control back to Miou until
there is an event. To do this, `Unix.select` can have `-1.0` as its last
argument. However, in our example, this is not very relevant. But for our `echo`
server, it's crucial that it does nothing but wait for system events. In this
regard, we recommend reading the implementation of `Miou_unix`, which closely
resembles what we have just done and handles the `block` option.

## Conclusion

You've finally completed this little tutorial, which demonstrates what is
arguably the most challenging part of Miou. It should be noted that we have
mentioned other possible solutions in our interaction with the system here and
there:
- starting with `epoll()`
- mentioning interruption mechanisms other than our `Unix.pipe`
- explaining a completely different design with `Stdlib.Thread` that could be
  more efficient

In addition to these, there are subtle differences between systems (even Unix),
and we realize that standardizing and homogenizing such a problem is a difficult
task. What is most important to take away from this tutorial is Miou's ability
to allow you to re-appropriate what is, for us, essential in the development of
a system and network application: interactions with the system.

We could claim (and have the audacity to) offer solutions that would cover 90%
of use cases in the development of such applications, but in our ambition to
create unikernels as a service, we definitely fall into the margin (the
remaining 10%). In this regard, we offer perhaps something that may be
rudimentary but accessible and usable for truly all use cases.

Finally, this book is not finished. As mentioned in the previous chapter, we
still need to cover Mutexes and Conditions. To do this, we will reuse our `echo`
server and improve it!

[^proof]: It is worth noting that the `Miou.Pqueue` module comes from an
extraction of an implementation of a proven priority queue using [Why3][why3]
(see the [vocal][vocal] project).

[^thread]: Another solution, which would be more challenging to implement but
more efficient, would be to use `Stdlib.Thread` to observe events. This
observation would occur concurrently with our domain and transmit signals via a
shared queue with our `select`. In this case, `select` would no longer be
blocking at all, and we would no longer need to implement an `interrupt`.

[tls]: https://en.wikipedia.org/wiki/Thread-local_storage
[why3]: https://www.why3.org/
[vocal]: https://github.com/ocaml-gospel/vocal
