# Miou, a simple scheduler for OCaml 5

```ocaml
let () = Miou.run @@ fun () ->
  print_endline "Hello World!"
```

Miou is a library designed to facilitate the development of applications
requiring concurrent and/or parallel tasks. This library has been developed with
the aim of offering a fairly simple and straightforward design. It's a pretty
small library with few dependencies that frames the behaviour of applications
using precise and conservative rules to guide users in their development.

The API documentation is available [here][documentation]. It describes (with
examples) Miou's behaviour. The official repository is available
[here][repository]. We also offer a mirror of this repository on
[GitHub][github]. The project is being maintained by the robur.coop cooperative.

Miou is focusing on 2 objectives:
- to provide a best-practice approach to the development of OCaml applications
  requiring concurrency and/or parallelism
- composability that can satisfy the most limited contexts, such as unikernels

Miou meets these objectives by:
- conservative and stable rules for the library's behaviour
- an API that delegates suspension management to the user

You can read a simple tutorial explaining how to implement an echo server with
Miou [here][echo].

### Rules

Miou complies with several rules that the user must respect. These rules (which
can be restrictive) help to guide the user towards good practice and avoid
*anti-patterns*. This notion of rules and anti-patterns is arbitrary
<sup>[1](#fn1)</sup> - it can therefore be criticised and/or disengage the
developer from using Miou. These rules come from our experience of system
programming in OCaml, where the development of our software today confirms
certain anti-patterns that we would not want to reproduce today (in view of the
technical debt that these bring).

#### Creating and waiting for a task

There are 2 ways of creating a task:
- it can run concurrently with other tasks and execute on the domain in which it
  was created (see `Miou.call_cc`)
- it can run in parallel with other tasks and be executed on **another** domain
  (see `Miou.call`)

The first rule to follow is that the user must wait for all the tasks he/she has
created. If they don't, Miou raises an exception: `Still_has_children`:
```ocaml
let () = Miou.run @@ fun () ->
  ignore (Miou.call_cc @@ fun () -> 42)
Exception: Miou.Still_has_children
```

The user must therefore take care to use `Miou.await` for all the tasks
(concurrent and parallel) that he/she has created:
```ocaml
let () = Miou.run @@ fun () ->
  let p0 = Miou.call_cc @@ fun () -> 42 in
  Miou.await_exn p0
```

#### Relationships between tasks

A task can only be awaited by the person who created it.
```ocaml
let () = Miou.run @@ fun () ->
  let p0 = Miou.call_cc @@ fun () -> 42 in
  let p1 = Miou.call_cc @@ fun () -> Miou.await_exn p0 in
  Miou.await_exn p1
Esxception: Miou.Not_a_child
```

This rule dictates that passing values from one task to another requires
(pragmatically) that a resource be allocated accordingly to represent such a
transmission. It also reaffirms that such a passage of values must surely be
protected by synchronisation mechanisms between the said tasks.

The only valid relationship (and transmission of values) between 2 tasks offered
by Miou is that between a child and its parent.

#### Abnormal termination

If a task fails (with an exception), all its sub-tasks also end.

```ocaml
let prgm () = Miouu.run @@ fun () ->
  let p = Miou.call_cc @@ fun () ->
    let q = Miou.call_cc @@ fun () -> sleep 1. in
    raise (Failure "p") in
  Miou.await p

let () =
  let t0 = Unix.gettimeofday () in
  let _  = prgm () in
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 < 1.)
```

This code shows that if `p` fails, we also stop `q` (which should wait at least
1 second). This shows that our `prgm` didn't actually last a second. Abnormal
termination will always attempt to complete all sub-tasks so that there are no
*zombie* tasks.

#### Wait or cancel

It was explained above that all children must be waited on by the task that
created them. However, the user can also `Miou.cancel` a task - of course, this
produces an abnormal termination of the task which automatically results in the
termination of all its children.

```ocaml
let () = Miou.run @@ fun () ->
  Miou.cancel (Miou.call_cc @@ fun () -> 42)
```

This code shows that if it is not possible to `ignore` the result of a task, it
is still possible to `cancel` it.

#### Randomised tasks

Tasks are taken randomly. That is to say that this code could return 1 as 2.
```ocaml
let prgm () =
  Miou.run @@ fun () ->
  let a = Miou.call_cc (Fun.const 1) in
  let b = Miou.call_cc (Fun.const 2) in
  Miou.await_first [ a; b ]

let rec until_its n =
  match prgm () with
  | Ok n' when n = n' -> ()
  | _ -> untils_its n

let () =
  until_its 1;
  until_its 2
```

This code shows that it is possible for our program to return 1 or 2. The reason
why we decided to randomly select the promises allows:
1) extend the coverage of your code
2) be less sensitive to predictions that could help an attacker

<hr>

<tag id="fn1">**1**</tag>: This arbitrary consideration proves that the answer
to the development of concurrent and/or parallel applications cannot be
absolute, and is based on individual affects and principles. Once again, we are
not suggesting that Miou is the ultimate solution to these problems, and we will
not commit ourselves to treating Miou as a viable solution from all points of
view.

We just believe that it corresponds to our problems and our points of view. It
is then up to the user to (dis)consider all this - which, as it stands, is much
more than a strictly technical description.

### Suspension and API

Miou finally proposes that the management of the suspension be delegated to the
user. Indeed, task management focuses mainly on suspension management: that is,
a task that can *block* the process.

It turns out that suspend mainly<sup>[2](#fn2)</sup> only affects the use of
resources offered by the system (sockets, files, time, etc.). Our experience in
system programming and in the development of unikernels teaches us that this
management of system resources, although intrinsic to task management, is:
- complex because of the subtleties that may exist between each system (Linux,
  \*BSD, Mac, Windows, unikernels)
- specific to the case of the suspension of a task while waiting for a signal
  from the system

As such and in our objective of composability with exotic systems, we have
decided to offer the user two libraries:
- `miou`, which is the core of our project
- `miou.unix`, which is an extension of our core with I/O

The second takes advantage of the API of the first regarding suspension. There
is a [tutorial][sleepers] explaining this API step by step and how to use it so
that you can manage everything related to suspension (and, by extension, your
system resources through the API it can offer).

<hr>

<tag id="fn2">**2**</tag>: It is noted that the suspension does not concern only
I/O and the resources of a system. Mutexes, conditions or semaphores can also
suspend the execution of a program. Our documentation and tutorials explain
those cases that we consider *marginal* in the interest of internalizing
suspension mecanism rather than exporting it to the user (but which are equally
important in the design of an application).

### A round-robin scheduler

Miou implements what is known as a round-robin scheduler. In other words, a
scheduler that gives equivalent opportunities to all tasks to consume a certain
amount of CPU time. This kind of task execution order management is intended to
improve the availability of tasks to be synchronised with system events.

The round-robin scheduler also allows us to avoid domain starvation problems.

However, we would like to warn the user that Miou does not correspond to all
applications. To find out more about this, we recommend that you read the
tutorial on implementing an application that calculates the hash of a folder:
[merkle-tree][merkle-tree].

## Genesis

The development of Miou began following discussions with a number of actors,
where we noted certain differences of opinion. We were not satisfied with the
different signals we received on the problem of scheduling in the OCaml
ecosystem, despite repeated efforts to reconcile these differences. Miou does
not present itself as the absolute solution to the scheduling problem. It is
simply the reemergence of these opinions in another environment which has
unfortunately not been offered by the actors who had the opportunity to do so.

We would like to make it clear that we do not want to monopolise and/or compete
with anyone. We would also like to inform future users that Miou regards our
objectives and our vision - which you may not agree with. So, if Miou satisfies
you in its approach (and that of its maintainers), and its objectives (and those
of its users), welcome!

[repository]: https://git.robur.coop/robur/miou
[github]: https://github.com/roburio/miou
[documentation]: https://roburio.github.io/miou/
[sleepers]: https://roburio.github.io/miou/miou/sleepers.html
[merkle-tree]: https://roburio.github.io/miou/miou/merkle.html
[merkle-tree]: https://roburio.github.io/miou/miou/echo.html
