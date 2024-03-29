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

[connect]: https://cr.yp.to/docs/connect.html
[io_uring]: https://en.wikipedia.org/wiki/Io_uring
[lwt]: https://github.com/ocsigen/lwt
[posix]: https://en.wikipedia.org/wiki/POSIX
