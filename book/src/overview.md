# Overview

Miou is an asynchronous runtime for the OCaml programming language. It provides
a simple library to build what is needed for networking applications. It gives
the flexibility to target any systems such as unikernel, large servers with
dozens of cores to small embedded devices.

At a high level, Miou is designed to provide:
- a multi-domain runtime for executing asynchronous code.
- the ability to use specific features of a system.
- space for users to extend the Miou ecosystem.

## Miou's role in your project

When you write your application in a asynchronous manner, you enable it to scale
much better by reducing the cost of doing many things at the same time. However,
asynchronous OCaml code does not run on its own, so you must choose a runtime
to execute it. The Miou library provides such runtime.

Miou offers domain management and the ability to run tasks in parallel. As far
as task management is concerned, users don't have to worry about synchronicity
between domains, Miou manages these issues for them. Miou offers the following
elements for parallel programming: `Miou.Mutex` & `Miou.Condition`.

Miou is system agnostic and requires only OCaml to run. This gives users the
opportunity to consider managing system events according to their own
constraints. However, Miou also offers a small extension that provides some
blocking _syscalls_ for the Miou runtime: `Miou_unix`.

Miou also offers task and resource management. It is possible to attach
resources to tasks and thus manage these resources in abnormal cases (exception
raising, cancellation).

## Advantage of Miou

Miou is very minimal. The advantage is that its minimality means you can really
'formalise' its behaviour without getting lost in an overly complex API. Only a
few fundamental elements are proposed and we leave users free to extend Miou
according to their contexts.

Thus, Miou can be extended to handle events in a more customised way. We believe
that the interaction between a program and the system is fundamentally
complicated and that it is pointless to think of homogenising such interaction.
Miou is therefore fairly transparent in what it requires and what it offers.
This may seem 'rudimentary' but we don't want any unpleasant surprises about
what we're proposing.

## When not to use Miou

Although Miou is useful for many projects that need to do a lot of things
simultaneously, there are also some use-cases where Miou is not a good fit.

* Speeding up CPU-bound computations by running them in parallel on several
  domains. Miou is designed for IO-bound applications where each individual task
  spends most of its time waiting for IO. If the only thing your application
  does is run computations in parallel, you should be using
  [moonpool][moonpool]. That said, it is still possible to "mix & match" if you
  need to do both.
* Do something dumb. Miou has a few rules to prevent you from doing anything
  _dumb_. So, if you want to code fast and have a program that works first time
  in your specific case, Miou may not provide the most pleasurable experience.
  Sometimes it's better to just play with [Unix][unix] for fun.

##

The user may have to prospect for the future of Miou in order to make the
decisive choice to use it... or not. There are a number of things we can say
about Miou. You can never say never, but we can agree on a certain direction for
Miou's development. So Miou will never be:

* A standard library. Miou is a key part of an application. It could also play
  the role of a standard library for the user (the best of both worlds in a
  single library). However, this would run counter to its minimalist nature and
  the ability of our users to make Miou their own. We'd rather leave space than
  monopolise it.
* A standard asynchronous library. The question of the synchronicity of tasks
  and values (in concurrency and/or in parallel) can be complex. There are, of
  course, 'default' solutions (protecting everything with Mutexes) but there are
  also solutions that can be subtly more interesting for the user. This is an
  area in itself (of solutions and research) where we wouldn't presume to have
  the last word.
