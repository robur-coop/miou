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
[here][repository]. The project is being maintained by the [robur.coop][robur]
cooperative.

Miou is focusing on 2 objectives:
- to provide a best-practice approach to the development of OCaml applications
  requiring concurrency and/or parallelism
- composability that can satisfy the most limited contexts, such as unikernels

Miou meets these objectives by:
- conservative and stable rules for the library's behaviour
- an API that delegates suspension management to the user

A [book][book] is available which explains how to make applications with Miou in
details. It introduces the reader to effects, implements a small scheduler and a
small echo server as an example. You an also read a simple tutorial from our
documentation explaining how to implement this echo server with [here][echo].

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
  was created (see `Miou.async`)
- it can run in parallel with other tasks and be executed on **another** domain
  (see `Miou.call`)

The first rule to follow is that the user must wait for all the tasks he/she has
created. If they don't, Miou raises an exception: `Still_has_children`:
```ocaml
let () = Miou.run @@ fun () ->
  ignore (Miou.async @@ fun () -> 42)
Exception: Miou.Still_has_children
```

The user must therefore take care to use `Miou.await` for all the tasks
(concurrent and parallel) that he/she has created:
```ocaml
let () = Miou.run @@ fun () ->
  let p0 = Miou.async @@ fun () -> 42 in
  Miou.await_exn p0
```

#### Relationships between tasks

A task can only be awaited by the person who created it.
```ocaml
let () = Miou.run @@ fun () ->
  let p0 = Miou.async @@ fun () -> 42 in
  let p1 = Miou.async @@ fun () -> Miou.await_exn p0 in
  Miou.await_exn p1
Exception: Miou.Not_a_child
```

This rule dictates that passing values from one task to another requires
(pragmatically) that a resource be allocated accordingly to represent such a
transmission. It also reaffirms that such a passage of values must surely be
protected by synchronisation mechanisms between the said tasks (with
`Miou.Mutex` or `Miou.Condition`).

The only valid relationship (and transmission of values) between 2 tasks offered
by Miou is that between a child and its parent.

#### Abnormal termination

If a task fails (with an exception), all its sub-tasks also end.

```ocaml
let prgm () = Miou_unix.run @@ fun () ->
  let p = Miou.async @@ fun () ->
    let q = Miou.async @@ fun () -> Miou_unix.sleep 1. in
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
  Miou.cancel (Miou.async @@ fun () -> 42)
```

This code shows that if it is not possible to `ignore` the result of a task, it
is still possible to `cancel` it.

#### Randomised tasks

Tasks are taken randomly. That is to say that this code could return 1 as 2.
```ocaml
let prgm () =
  Miou.run @@ fun () ->
  let a = Miou.async (Fun.const 1) in
  let b = Miou.async (Fun.const 2) in
  Miou.await_first [ a; b ]

let rec until_its n =
  match prgm () with
  | Ok n' when n = n' -> ()
  | _ -> until_its n

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
suspension mechanism rather than exporting it to the user (but which are equally
important in the design of an application).

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

## Memento

If readers are interested in the OCaml ecosystem and the role Miou could play
in it, we would also like to refer them to this [discuss post][discuss], which
summarises our objectives and how we would like to become part of the OCaml
community.

We would also like to affirm that our position is always one of collaboration
with others. We do not impose the use of Miou (as we do little to promote it),
but we would not want anyone to impose their solution on us without prior
discussion and a search for compromise (as we systematically do when
participating in projects that do not belong to us).

Since the beginning of OCaml 5 and the appearance of schedulers, we have seen
very little collaborative approach to finding a solution and have mainly
noticed behaviour that is at best dogmatic and sometimes problematic (according
to [the OCaml CoC][coc]). As such, although we remain open, we no longer
necessarily have the energy to promote and build bridges with other schedulers.
However, we would be delighted to continue improving the experience of our
users and the OCaml community in general.

[repository]: https://github.com/robur-coop/miou
[github]: https://github.com/robur-coop/miou
[documentation]: https://docs.osau.re/miou/
[sleepers]: https://docs.osau.re/miou/sleepers.html
[echo]: https://docs.osau.re/miou/echo.html
[robur]: https://robur.coop/
[book]: https://robur-coop.github.io/miou
[discuss]: https://discuss.ocaml.org/t/on-concurrency-models/15899/13?u=dinosaure
[coc]: https://ocaml.org/policies/code-of-conduct
