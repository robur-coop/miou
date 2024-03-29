# Retrospective

At this stage, you're familiar with all the concepts of a scheduler,
asynchronous programming, and interactions with the system. Of course, as you
might suspect, we've omitted a whole bunch of details, and Miou offers much more
than our simple scheduler. However, at this point, we can describe in detail
what Miou brings to the table (including its subtleties). That's what we'll
explore in this chapter.

## A task as a resource

If you recall from the example of our `echo` server, we wanted to handle client
management in the background:
```ocaml
    ignore (spawn @@ fun () -> echo client)
```

It's possible to do the same thing with `Miou.call_cc`. This function will more
or less do the same thing as our `spawn`, namely: create a new task that will
run on the same _thread_ using our scheduler. This type of task that coexists
with others on the same thread is called a _fiber_. And, just like `spawn`,
`Miou.call_cc` returns a promise to this task. In Miou, you're creating a child
of your main task, a subtask.

The fundamental difference with Miou is that you can't forget about its
children!
```ocaml
let () = Miou.run @@ fun () ->
  ignore (Miou.call_cc @@ fun () -> print_endline "Hello World!")
Exception: Miou.Still_has_children.
```

We consider in Miou that a task is a resource. You allocate it (using
`Miou.call_cc`) but you also have to free it with `Miou.await`. A fundamental
rule governs a program made with Miou: all tasks must either be awaited or
canceled. Forgetting a task will result in a fatal error.

### Background tasks

This raises another question: what to do with my subtasks that manage clients?
In reality, if this rule exists, it's because these children can go wrong
(probably an education problem). And you need to be notified of these abnormal
situations. What interests you is not the existence of these tasks (because your
goal is to put them in the background) but the result of these tasks to know if
everything went well.

In this regard, Miou offers a way to save your tasks somewhere and retrieve them
if they have finished periodically. This is mainly thanks to the `orphans`
value:
```ocaml
let rec clean_up orphans = match Miou.care orphans with
  | None | Some None -> ()
  | Some (Some prm) ->
    match Miou.await prm with
    | Ok () -> clean_up orphans
    | Error exn -> raise exn

let server () =
  ...
  let orphans = Miou.orphans () in
  while true do
    clean_up orphans;
    let client, address_of_client = Miou_unix.accept socket in
    ignore (Miou.call_cc ~orphans @@ fun () -> echo client)
  done
```

The advantage of such an approach is to consider a task as a resource that must
be freed at some point in your program. Our experience in implementing protocols
at [Robur][robur] convinces us that one should not forget about their children.
Developing system and network applications involves creating programs whose
execution time is long (6 months, 1 year, etc.). A task is a resource that uses
memory and possibly your processor. If you allow yourself to forget your tasks,
they will continue to exist in memory, and an accumulation of this forgetfulness
will inevitably result in a memory leak that will hinder the existence of your
program in the long term (the system will kill your program with an [OOM][oom]).

## Multiple domain runtime

In the introduction, it was mentioned that it's possible to use multiple domains
with Miou. Indeed, since OCaml 5, it has been possible to launch tasks in
parallel. This parallelism has become possible only recently because these tasks
have their own _minor heap_. Thus, synchronization between domains regarding
allocation and garbage collection is less systematic.

To launch a task in parallel with Miou, it's sufficient to use:
```ocaml
let () = Miou.run @@ fun () ->
  let prm = Miou.call @@ fun () ->
    print_endline "My parallel task." in
  Miou.await_exn prm
```

Miou takes care of allocating multiple domains according to your system's
specifics. These domains will be waiting for tasks, and `Miou.call` notifies
them of a new task to perform. Just like `Miou.call_cc`, `Miou.call` also
returns a promise, and the same rules apply: you must not forget about your
children.

A task in parallel explicitly means that it will run in a different domain than
the one it was created in. That is, this code, which returns the domain in which
the task is executing, is always true:
```ocaml
let () = Miou.run @@ fun () ->
  let p =
    Miou.call @@ fun () ->
    let u = Stdlib.Domain.self () in
    let q = Miou.call @@ fun () -> Stdlib.Domain.self () in
    (u, Miou.await_exn q)
  in
  let u, v = Miou.await_exn p in
  assert (u <> v)
```

However, the choice of the domain responsible for the task is made randomly.
Thus, this code is also true (meaning that two tasks launched in parallel
subsequently can use the same domain):
```ocaml
let () = Miou.run @@ fun () ->
  let assertion = ref false in
  while !assertion = false do
    let p = Miou.call @@ fun () -> Stdlib.Domain.self () in
    let q = Miou.call @@ fun () -> Stdlib.Domain.self () in
    let u = Miou.await_exn p in
    let v = Miou.await_exn q in
    assertion := u = v
  done
```

It may happen then that we want to distribute a specific task to all our
available domains. We cannot do this with `Miou.call`, which may, several times,
assign the same domain for the task. However, Miou offers a way to distribute
the workload evenly across all your domains:
```ocaml
let task () : int = (Stdlib.Domain.self () :> int)

let () = Miou.run ~domains:3 @@ fun () ->
  let domains =
    Miou.parallel task (List.init 3 (Fun.const ()))
    |> List.map Result.get_ok in
  assert (domains = [1; 2; 3])
```

Finally, one last rule exists regarding parallel tasks. There may be a situation
called starvation. Indeed, like your number of cores, the number of domains is
limited. It may happen that domains wait for each other, but it's certain that
the main domain (the very first one that executes your code, known as `dom0`)
will never be assigned a task.

This rule prevents a domain from waiting for another domain, which waits for
another domain, which waits for `dom0`, which waits for your first domain - the
[starvation problem][starvation]. Thus, it may happen that `dom0` is no longer
involved in the execution of your program and is only waiting for the other
domains. However, we can involve it using `Miou.call_cc`:

```ocaml
let task () : int = (Stdlib.Domain.self () :> int)

let () = Miou.run ~domains:3 @@ fun () ->
  let prm = Miou.call_cc my_super_task in
  let domains =
    Miou.await prm
    :: Miou.parallel task (List.init 3 (Fun.const ())) in
  let domains = List.map Result.get_ok domains in
  assert (domains = [0; 1; 2; 3])
```

However, we would like to warn the user. Parallelizing a task doesn't
necessarily mean that your program will be faster. Once again, Miou focuses
essentially on managing system events. Domains are equally subject to observing
these events, which means that all computations are interleaved with this
observation (and can therefore have an impact on performance).

Having more domains is not a solution for performance either. If Miou takes care
of allocating (as well as releasing) domains, it's because they require a lot of
resources (such as a minor-heap). Thus, the domains are available, but it's Miou
that manages them.

Finally, Miou mainly aims to facilitate the use of these domains, especially
regarding inter-domain synchronization. Indeed, tasks as well as their promises
concerning `Miou.call` do not exist in the same domains. An internal mechanism
helps the user not to worry about the synchronicity between the task's state and
its promise, even though they exist in two spaces that run in parallel.

## Interaction with the system

You may not have noticed, but our example of implementing an asynchronous server
has structured itself around two questions:
1) the scheduler
2) interactions with the system

Miou maintains a clear separation between the scheduler (the `Miou` module) and
interactions with the Unix system (the `Miou_unix` module). This choice, not so
strange considering what [lwt][lwt] can offer, is intended for two reasons:
1) to be able to use Miou with exotic systems such as a unikernel where the Unix
   module (or the [POSIX][posix] interface) is unavailable.
2) to delegate the complexity of system interactions to the user.

Regarding our first argument, it naturally concerns the objectives that our
cooperative has, namely developing and deploying unikernels as services (such as
this website, which is a unikernel!).

The second argument may seem ungrateful because it delegates the responsibility
of implementing system interactions to our users. However, we believe that this
issue is fundamentally complex, and it is futile to try to standardize anything
at this level.

To support our argument, one only needs [to ask][connect] about the behavior of
the `connect()` function (to establish a connection with a server) on different
systems like Linux or \*BSD. More generally, systems all have more or less
different behaviors and are not necessarily well-documented. Users also may want
to take advantage of certain specificities of their system (such as
[io_uring][io_uring]), and we do not claim (nor have the resources) to know both
all the subtleties between systems and your use cases.

Our approach is therefore to let you take back these questions. Miou offers a
way (as we've seen with our small scheduler) to suspend and resume functions:
- Suspension is possible via `Miou.suspend`.
- Notification to Miou that an event allows unblocking a task is done by
  injecting a `Miou.event` value via the `Miou.run` function.

We recommend reading our tutorial on sleepers to become more familiar with this
interface. However, for those whose focus is not central, Miou offers the
`Miou_unix` module, which implements some essential syscalls in the development
of system and network applications.

## Cancellation

One aspect we intentionally omitted during the implementation of our scheduler
is task cancellation. Why would we need to cancel a task? There are various 
reasons why an event might not occur. For instance, consider trying to connect
to a server that is unavailable. We wouldn't want to attempt this connection
indefinitely and might want to cancel it after a certain time, like so:
```ocaml
exception Timeout

let connect_or_fail address =
  let socket = match address with
    | Unix.ADDR_UNIX _ -> failwith "Invalid address"
    | Unix.ADDR_INET (inet_addr, _) ->
      if Unix.is_inet6_addr inet_addr
      then Miou_unix.tcpv6 ()
      else Miou_unix.tcpv4 () in
  let sleep = Miou.call_cc @@ fun () -> Miou_unix.sleep 5.0; raise Timeout in
  let prm = Miou.call_cc @@ fun () -> Miou_unix.connect socket address in
  match Miou.await_first [ sleep; prm ] with
  | Ok () -> Ok socket
  | Error Timeout -> Miou_unix.close socket; Error `Timeout
  | Error exn -> Miou_unix.close socket; raise exn
```

Here, `Miou.await_first` will provide the result of the first finished task.
Then, it cancels all other tasks (with `Miou.cancel`). As mentioned, we cannot
forget our children: we must either await them or cancel them. In this case,
`Miou.await_first` awaits one and cancels all others - we continue to adhere to
our rule.

Cancellation becomes significantly more complex when domains are introduced. It
requires strong synchronicity between tasks and their promises. Cancellation
does not immediately terminate a task; this is especially true if the task runs
in parallel, where a synchronization mechanism is necessary.

Miou ensures that **after** a `Miou.cancel`, the task has indeed been canceled.
During cancellation, the scheduler might still execute a portion of the task
(again, particularly true if it runs in parallel). However, Miou tries its best
to effectively cancel the requested tasks.

Since canceling a task requires a mechanism that synchronizes the task and its
promise's state, during this process, the scheduler might take the opportunity
to execute other tasks. This is why we say cancellation is _asynchronous_ as
well. For example, note that `p0` runs despite the cancellation of `p1`:
```ocaml
let () = Miou.run @@ fun () ->
  let p1 = Miou.call @@ fun () -> Miou.yield () in
  let v = ref false in
  let p0 = Miou.call_cc @@ fun () -> print_endline "Do p0" in
  print_endline "Cancel p1";
  Miou.cancel p1;
  print_endline "p1 cancelled";
  Miou.await_exn p0

(* This program prints:
   Cancel p1
   Do p0
   p1 cancelled
*)
```

We can also cancel a task that has finished. In this case, the cancellation
takes precedence over resolution (thus, we lose the result of our task).
```ocaml
let () = Miou.run @@ fun () ->
  let prm = Miou.call @@ fun () -> 1 + 1 in
  let _ = Miou.await prm in
  Miou.cancel prm;
  match Miou.await prm with
  | Ok _ -> assert false
  | Error Miou.Cancelled -> assert true
  | Error exn -> raise exn
```

Lastly, it's worth noting that cancellation affects not only a task but also all
its children:
```ocaml
let rec infinite () = infinite (Miou.yield ())

let () = Miou.run @@ fun () ->
  let p = Miou.call_cc @@ fun () ->
    let q = Miou.call infinite in
    Miou.await_exn q in
  Miou.cancel p
```

## Mutex & Condition

After this detailed presentation of Miou, we can realize that the announced
rules imply a rather strict usage of tasks and promises. However, when it comes
to asynchronous and/or parallel programming, we need elements to transmit
information between tasks.

However, Miou only allows one type of transmission between tasks: between
children and their direct parents. So, how can we share information between two
parallel tasks that do not have this parent-child relationship?

The real question here is the synchronicity between two tasks executing in
parallel or that can run concurrently. Of course, there are myriad solutions
(and research) available depending on the context and your objectives. Similar
to system interaction, we do not claim to know everything about this broad
domain. However, we know that several basic elements exist to assist you:
- Atomic values (provided by OCaml)
- Mutexes to obtain exclusive control over a resource
- Conditions allowing suspension and continuation if a predicate related to a
  resource is true

One difference to note, however: Miou offers Mutexes and Conditions, but so does
OCaml. Which ones should you use then? If you are using Miou, you should use our
implementations. OCaml's Mutexes and Conditions will work (to some extent), but
they will not suspend a particular task but your entire domain. These OCaml
elements also lack awareness of cancellation (`Miou.cancel`).

In other words, you should use `Miou.Mutex` and `Miou.Condition` if you are
using Miou. However, the general behavior of these is almost equivalent to what
OCaml offers (if we ignore cancellation).

[robur]: https://robur.coop/
[oom]: https://fr.wikipedia.org/wiki/Out_of_memory
[starvation]: https://en.wikipedia.org/wiki/Starvation_(computer_science)
[connect]: https://cr.yp.to/docs/connect.html
[io_uring]: https://en.wikipedia.org/wiki/Io_uring
[lwt]: https://github.com/ocsigen/lwt
[posix]: https://en.wikipedia.org/wiki/POSIX
