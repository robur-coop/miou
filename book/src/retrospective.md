# Retrospective

At this stage, you're familiar with all the concepts of a scheduler,
asynchronous programming, and interactions with the system. Of course, as you
might suspect, we've omitted a whole bunch of details, and Miou offers much more
than our simple scheduler. However, at this point, we can describe in detail
what Miou brings to the table (including its subtleties). That's what we'll
explore in this chapter.

## A task as a resource

Let's revisit our example with the `echo` server, where we aimed to handle
client management in the background:
```ocaml
    ignore (spawn @@ fun () -> echo client)
```

You can achieve the same thing with `Miou.async`. This function essentially
does will more what our `spawn` does: it creates a new task that will run on the
same _thread_ using our scheduler. This type of task which coexists with others
on the same thread is called a _[fiber][fiber]_. And, just like `spawn`,
`Miou.async` also returns a promise for this task. In Miou, you're creating a
child of your task, a subtask.

The key difference with Miou, though, is that you can't forget your children!
```ocaml
let () = Miou.run @@ fun () ->
  ignore (Miou.async @@ fun () -> print_endline "Hello World!")
Exception: Miou.Still_has_children.
```

In Miou, we treat a task as a resource. You allocate it (using `Miou.async`),
but you also have to release it with `Miou.await`. A fundamental rule governs
Miou programs: all tasks must either be awaited or canceled. Forgetting a task
will result in a fatal error.

### Background tasks

This brings up another question: what should we do with our subtasks that manage
clients? If this rule exists, it's because these children can misbehave. And you
need to be notified of these abnormal situations. What matters isn't the
existence of these tasks (since your goal is to put them in the background) but
their results to ensure everything went well.

In this regard, Miou offers a way to save your tasks somewhere and retrieve them
once they're completed. This is mainly done using the `orphans` value:
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
    ignore (Miou.async ~orphans @@ fun () -> echo client)
  done
```

The advantage of this approach is that it treats a task as a resource that must
be released at some point in your program's execution. Our experience in
implementing protocols at [Robur][robur] has convinced us of the importance of
not forgetting about our children. Developing system and network applications
involves creating programs with long execution time (6 months, 1 year, etc.).
Tasks consume memory and possibly processor resources. Forgetting tasks can lead
to memory leaks, which can hinder your program's long-term viability (the
system might terminate your program due to an [out-of-memory][oom] error).

## Structured concurrency

Managing tasks and their promises can be a real challenge when implementing a
large application. Indeed, conceptualizing tasks running in the background
leaves room for practices (like forgetting these tasks) that can lead to
significant maintenance overhead in the long run. At Robur, through certain
projects we maintain, we've encountered situations where the time to fix bugs
becomes disproportionately large given our resources because we need to
re-establish the mental model of task management, which isn't all that obvious.

Thus, when developing Miou, it was essential from the outset to establish rules
to prevent repeating past mistakes. We've already introduced one rule: never
forget your children.

There's a second rule: only the **direct** parent can wait for or cancel its
children. For instance, the following code is incorrect:
```ocaml
let () = Miou.run @@ fun () ->
  let a = Miou.async @@ fun () -> Miou.yield () in
  let b = Miou.async @@ fun () -> Miou.await_exn a in
  Miou.await_exn a;
  Miou.await_exn b
Exception: Miou.Not_a_child.
```

The purpose of such a constraint is to maintain a simple mental model of the
active tasks in your application: their affiliations form a tree with the root
being your main task (the one launched with `Miou.run`). Therefore, if your main
task terminates, it invariably means that all sub-tasks have also terminated.

## Cancellation

One aspect deliberately left out in the implementation of our small scheduler is
cancellation. It can be useful to cancel a task that, for example, is taking too
long. Miou provides this mechanism for all tasks using their promises.
```ocaml
let () = Miou.run @@ fun () ->
  let prm = Miou.async @@ fun () -> print_endline "Hello World" in
  Miou.cancel prm
```

The rules of parentage explained earlier also apply to cancellation. You can
only cancel your direct children:
```ocaml
let () = Miou.run @@ fun () ->
  let a = Miou.async @@ fun () -> Miou.yield () in
  let b = Miou.async @@ fun () -> Miou.cancel a in
  Miou.await_exn a;
  Miou.await_exn b
Exception: Miou.Not_a_child.
```

Cancellation overrides any promise state. You can cancel a task that has already
completed. In this case, we lose the result of the task, and it's considered
canceled:
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

Of course, to be consistent with our other rules, canceling a task implies
canceling all its sub-tasks:
```ocaml
let rec infinite () = infinite (Miou.yield ())

let () = Miou.run @@ fun () ->
  let p = Miou.async @@ fun () ->
    let q = Miou.call infinite in
    Miou.await_exn q in
  Miou.cancel p
```

Lastly, let's delve into the behavior of `Miou.cancel`. It's said that this
function is _asynchronous_ in the sense that cancellation (especially that of a
task running in parallel) may take some time. Thus, Miou seizes the opportunity
to execute other tasks during this cancellation. For example, note that `p0`
runs despite the cancellation of `p1`:
```ocaml
let () = Miou.run @@ fun () ->
  let p1 = Miou.call @@ fun () -> Miou.yield () in
  let v = ref false in
  let p0 = Miou.async @@ fun () -> print_endline "Do p0" in
  print_endline "Cancel p1";
  Miou.cancel p1;
  print_endline "p1 cancelled";
  Miou.await_exn p0
```
```shell
$ ocamlfind opt -linkpkg -package miou main.ml
$ ./a.out
Cancel p1
Do p0
p1 is cancelled
```

The advantage of this asynchronicity is to always be able to handle system
events even if we attempt to cancel a task.

## Multiple domain runtime

In the introduction, it was mentioned that it's possible to use multiple domains
with Miou. Indeed, since OCaml 5, it has been possible to launch functions in
parallel. This parallelism has become possible only recently because these
functions have their own _minor heap_. Thus, synchronization between domains
regarding allocation and garbage collection is less systematic.

To launch a task in parallel with Miou, it's sufficient to use:
```ocaml
let () = Miou.run @@ fun () ->
  let prm = Miou.call @@ fun () ->
    print_endline "My parallel task." in
  Miou.await_exn prm
```

Miou takes care of allocating multiple domains according to your system's
specifics. These domains will be waiting for tasks, and `Miou.call` notifies
them of a new task to perform. Just like `Miou.async`, `Miou.call` also
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
Thus, this code is also true (meaning that two tasks launched in succession
can then use the same domain):
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
assign the same domain for a task. However, Miou offers a way to distribute the
workload evenly across all your domains:
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
will never be assigned a task via `Miou.call`.

This rule prevents a domain from waiting for another domain, which waits for
another domain, which waits for `dom0`, which waits for your first domain - the
[starvation problem][starvation]. Thus, it may happen that `dom0` is no longer
involved in the execution of your program and is only waiting for the other
domains. However, we can involve it using `Miou.async`:

```ocaml
let task () : int = (Stdlib.Domain.self () :> int)

let () = Miou.run ~domains:3 @@ fun () ->
  let prm = Miou.async my_super_task in
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

## For the next steps

This retrospective allows us to introduce the basic elements of Miou. We now
need to see how to use Miou and also introduce you to some new concepts. The
next chapter will consist of re-implementing our echo server with Miou. There
should be only a few differences, but we will seize the opportunity to improve
our server, especially with the use of parallel tasks and the notion of
ownership.

[robur]: https://robur.coop/
[oom]: https://fr.wikipedia.org/wiki/Out_of_memory
[starvation]: https://en.wikipedia.org/wiki/Starvation_(computer_science)
[fiber]: https://en.wikipedia.org/wiki/Fiber_(computer_science)
