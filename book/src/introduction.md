# Miou, a simple scheduler for OCaml 5

Miou is a small library that facilitates asynchronous and parallel programming
in OCaml. It is a project of the [Robur][robur] cooperative, aimed at developing
system and network applications. This library only requires OCaml 5 and can be
obtained via [opam][opam].
```shell
$ opam install miou
```

Miou offers three key features:
- a multi-domain runtime for executing asynchronous code.
- flexibility in defining interactions between the system and Miou.
- essential components for asynchronous and/or parallel programming.

## Miou's role in your project

When developing an application that heavily interacts with the system to offer
services like an HTTP server, embracing asynchronous programming is recommended.
However, OCaml lacks built-in support for asynchronous programming,
necessitating the use of a library such as Miou, which provides the required
runtime.

Such a runtime emerges as a pivotal element of your application, orchestrating
and executing tasks to ensure continued service availability despite varying
workloads. Miou caters to diverse systems, ranging from unikernels to large
servers with numerous cores or even small embedded devices.

While pivotal, this runtime represents the final frontier between your
application's intended functionality and its current execution by interacting
with the system. Hence, we advise users to defer selecting the scheduler, such
as Miou, until the application's design phase is complete.

### A multi-domain runtime

Since OCaml 5, it has been possible to execute tasks in parallel. Miou provides
this capability by solving the inter-domain synchronization problems involved.
Miou allocates multiple domains that are available to the user to manage in
parallel, for example, clients.

We recommend referring to the OCaml manual to learn more about domains. Indeed,
Miou manages domains itself because they can be a costly resource. As such, Miou
handles their allocation, transfers your tasks to them, manages synchronization
when you want to obtain the results of your tasks, and ultimately deallocates
these domains properly.

### Agnostic to the system

Miou only requires OCaml to operate. This choice stems from our ambition to
integrate Miou as a scheduler for our unikernels, which are highly specialized
systems. However, more generally and based on experience, we understand that
interactions with the system are inherently complex and cannot be standardized
through a common interface.

Therefore, we believe that the best person to determine how to interact with
the system is you! Miou thus provides this capability so that you can leverage
the full potential of your system.

However, Miou offers a small extension allowing interaction with your system
through the `miou.unix` library. While rudimentary, it is adequate for most
system and network applications.

### Essential components for asynchronous/parallel programming

Finally, Miou provides essential elements for parallel and/or asynchronous
programming. These components help address synchronization challenges inherent
in parallel and/or asynchronous programming.

It is worth noting that these elements may seem somewhat rudimentary. However,
we would like to caution the user that the topic of synchronization is a vast
realm of solutions and research, and we do not claim to have omniscience over
it. Therefore, we prefer to leave this space open for the user.

## When not to use Miou

Although Miou is useful for many projects that need to do a lot of things
simultaneously, there are also some use-cases where Miou is not a good fit:
speeding up CPU-bound computations by running them in parallel on several
domains. Miou is designed for IO-bound applications where each individual task
spends most of its time waiting for IO. If the only thing your application
does is run computations in parallel, you should be using [moonpool][moonpool].
That said, it is still possible to "mix & match" if you need to do both.

[opam]: https://opam.ocaml.org
[robur]: https://robur.coop
[moonpool]: https://github.com/c-cube/moonpool
