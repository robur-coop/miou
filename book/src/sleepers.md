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
It's akin to what `Unix.sleep` does: aiting for a certain amount of time. We'll
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
would be `Miou.suspend` syscall. Therefore, our main goal is to save our syscall
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

Using a priority queue will allow us to get the syscall that needs to resume its
function as soon as possible among all our sleepers.

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

let event _ = { select; interrupt= ignore }
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

[tls]: https://en.wikipedia.org/wiki/Thread-local_storage
