(** {1 Miou, a simple scheduler for OCaml 5.}

    Miou is a simple scheduler for OCaml 5. The purpose of this library is to
    provide an interface for launching tasks concurrently and/or in parallel
    with others. Miou proposes a minimal, homogeneous, simple and conservative
    interface.
    
    This presentation will allow us to introduce several concepts and what Miou
    can offer.
    
    {2 Definitions of terms.}
    
    {3 A task.}
    
    A task is a function on which several operations required by Miou exist:
    + a task can be launched/executed
    + a task can be stopped (suspended)
    + stopping a task produces a state of the task
    + a task can be restarted from its state
    
    The {!module:State} module offers a more concrete definition of these
    operations with what the OCaml 5 {!module:Effect} module offers. Suspension
    is the fundamental operation in the implementation of a scheduleur. We have
    therefore chosen to be able to suspend a task as soon as it emits an effect.
    
    This is perhaps the most important thing to remember about this definition:
    a task is suspended as soon as it emits {b one} effect.
    
    {3 Suspension.}
    
    Suspension consists of obtaining a {i state} for the task which we can be
    "stored" and which allows us to {i continue} the task. The task has stopped
    at a specific point and its state consists roughly of:
    - the point where the task stopped
    - a particular state of the memory required to perform the task
    - and the disruptive element of the suspension: the effect.
    
    From this state, Miou can continue or {i discontinue} the task. Continuing
    the task consists of restarting the execution of the task from its stopping
    point. Discontinuing a task consists of not restarting the task and
    transitioning the state of the task to an error (in this case, the raising
    of an exception).
    
    {3:quanta A quanta.}
    
    A {i quanta} is a measure used to limit the execution of a task. Usually,
    the {i quanta} used is time: you limit the execution time of a task to
    100ms, for example, and then suspend the task and execute another one.
    
    As far as Miou is concerned, the {i quanta} used is the production of
    effects. For more details on the reasons for this, please refer to the
    {!module:State} module. The user can modify the number of {i quantas}
    allocated to tasks. By default, Miou only allocates one {i quanta} per task
    so that they can be reordered later. However, it may be appropriate for a
    task to consume a maximum of 10, 20 or 1000 {i quantas}.
    
    {3 Concurrency.}
    
    Concurrency consists of swapping the execution of several tasks on the same
    core. The aim of the scheduler is to have several tasks, some of which
    depend on the result of others, and to schedule these tasks in a certain
    execution order in order to obtain the final result.
    
    It is important to note that, in the context of concurrency, tasks have
    exclusive access to memory as only one core is able to execute them: there
    cannot be 2 tasks executing at the same time. However, the order in which
    the tasks are executed is decided by the scheduler.
    
    The policy of concurrency can be to "prioritise" some tasks so that other
    tasks, which depend on the result of the first tasks, can be unblocked. In
    our case, Miou does {b not} prioritise tasks but has a re-scheduling and
    execution policy which ensures that all tasks have the same opportunity to
    be executed (and that those producing a result needed by others can be
    executed just as well as the others).
    
    The user can launch a concurrently-running task using {!val:call_cc}. The
    function returns {i a promise} ({!type:t}), {i a witness} to the execution
    of the task. This witness can be used to obtain the result of the task:

    {[
      # Miou.run @@ fun () ->
        let promise = Miou.call_cc @@ fun () -> 21 + 21 in
        Miou.await_exn promise ;;
      - : int = 42
    ]}

    {3 Parallelism.}
    
    Since OCaml 5, it has been possible to run functions in parallel. In other
    words, they can run "at the same time" using several cores. The advantage of
    parallelism is that execution time can be shared between several cores. For
    example, if 2 tasks require 100ms to calculate a result, in a concurrent
    context we would need 200ms to complete these tasks, whereas we would only
    need 2 cores and 100ms to complete them in parallel.
    
    Earlier, we mentioned exclusive access to memory by tasks if they are
    concurrent. Unfortunately, this is no longer true in parallel. If you want
    to keep this property for certain values, you should use the
    {!module:Atomic} module.

    The user can launch tasks in parallel using {!val:call}. Note that the
    {i witness} for the parallel task (the promise {!type:t}) is of the same
    type as that produced by {!val:call_cc}.

    We recommend that you let Miou decide how many domains to allocate.
    
    {4 Domains.}
    
    Miou is able to use several cores and thus launch tasks in parallel because
    it is able to create {!type:Domain.t}. However, the number of domains is
    {b limited}: it is counter-productive to launch 10 domains when we only have
    (physically) 4 cores, for example.

    Miou also differentiates between [dom0] (the main domain that runs your
    program) and the other domains. The main difference is that {!val:call} (or
    {!val:parallel}) will {b never} assign a new task in parallel to [dom0].
    
    {3 Synchronisation points.}
    
    We mentioned earlier that some tasks can "wait" for the results of other
    tasks. We call these "synchronisation points". Since tasks can run
    concurrently and/or in parallel, Miou offers functions where a particular
    state of the tasks (the termination state) is expected.
    
    Miou will then be in a waiting state (it will simply observe the state of
    the said task until this state has changed):
    - while waiting for a concurrent task, Miou will then re-schedule and
      execute other tasks in order to "unblock" the first one
    - while waiting for a task to run in parallel, Miou will suggest that other
      tasks assigned to the same domain can run while the first one continues to
      run (on another domain) and return to a waiting state until the task in
      question has finished.

    It may be possible to wait for one ({!val:await}) or more tasks
    ({!val:await_all}) using their promises. It is also possible to wait for one
    of the available tasks ({!val:await_first} or {!val:await_one}). The result
    of the first task to finish will be given when it is used.
    
    {3 System events.}
    
    Another waiting state exists: waiting for a system event, such as waiting
    for a TCP/IP connection to arrive. Miou provides the ability for users to
    implement these system event synchronisation points themselves. We recommend
    reading the implementation of {!page:sleepers} with Miou to find out more
    about this.
    
    {!module:Miou_unix} implements some of these points, such as waiting to
    receive information ([read]) or waiting to be able to write information
    ([write]), as well as other system events.
    
    What makes these points of synchronisation of system events different from
    waiting for the result of a {i pure} task (which does not interact with the
    system) is that we cannot calculate the {i waiting time}. We can wait a few
    milliseconds or 1 hour for the arrival of a TCP/IP connection, for example.
    
    This makes it difficult to prioritise tasks in relation to each other, as
    we lack too much information to find the optimum order for executing tasks.
    Once again, Miou doesn't prioritise tasks.
    
    {2 The "round-robin" scheduler.}
    
    Miou implements what is known as a round-robin scheduler. This is a
    scheduler with very simple rules:
    + if a task arrives, execute it up to a certain {i quanta}
    + if the task has finished, give the result
    + if not, re-order the task at the end of the to-do list
    + take the next task and repeat the operation.
    
    The special feature of a round-robin scheduler is that it does not
    prioritise tasks according to their status. It simply allocates {b a fair}
    amount of time/{i quanta} of domain usage to all tasks (a bit like
    communism).
    
    So, by default, Miou suggests that a task can only emit one and only one
    effect, which is our {!section:quanta}. Most of the functions proposed by
    Miou produce an effect. Miou then reorders the task at the end of the to-do
    list and repeats the operation.
    
    {3 Availability.}
    
    The advantage of this type of task management policy is that it increases
    the availability of tasks. For example, 2 tasks waiting for 2 system events
    (the reception of a TCP/IP packet and the waiting for a new TCP/IP
    connection) will have the same execution time allocated to them.
    
    This availability means that Miou is more {i in-sync} with system events. In
    fact, the system keeps these events until the application space requests
    them (with [select()]) and consumes them ([read()], [accept()], etc.).
    Miou's objective is to ensure that several tasks (dependent on these events)
    can all respond to the consumption of these events from the system, without
    one of them being able to have exclusive execution time on a domain.
    
    In this way, a Miou application can respond to the consumption of a [read()]
    and an [accept()] without one of these tasks blocking the other - even
    though the two correspond to completely different execution paths. Finally,
    a Miou application {b is} available from a system and network point of view.
    
    {3 Time wasted.}
    
    The disadvantage of such a policy is the execution of pending tasks. This is
    because Miou does not discriminate between tasks: it does not prioritise
    tasks that can do something over those that are waiting.
    
    So it could happen that Miou "wastes" its time trying to execute pending
    tasks for 1 {i quanta} and that the result of this execution comes to
    nothing (because the result is not yet available).
    
    This non-discriminatory approach is important because if we consider waiting
    for system events, it becomes difficult to prioritise tasks fairly since, by
    definition, system events can occur at any time. Miou therefore responds to
    the availability of tasks to consume system events. It does not address the
    optimal scheduling of {i pure} tasks.
    
    {3 The famine problem.}
    
    The prioritisation of tasks coupled with the limited use of domains can lead
    to a starvation problem. Indeed, through prioritisation, a task can be
    excluded from using one of the available domains - because it has been
    decided that other tasks have priority there. However, this excluded task
    may be necessary (and even central) to the completion of our program.
    
    In this case, we talk about a starvation problem. The round-robin scheduler
    solves this problem by not discriminating between tasks and by allocating
    them a fair execution time on the domains. The round-robin scheduler is
    {i starvation-free}. Even if it appears that Miou wasting time executing
    tasks that would not produce any results, the central task required to
    terminate our program would be invariably run in all cases.
    
    {3 Tuning.}
    
    It is possible to modify Miou's behaviour depending on the purpose of your
    program. Choosing to allow a task to emit only one effect can have serious
    implications for the application's performance. Miou therefore suggests that
    the user can decide how many {i quantas} that tasks can consume.
    
    In this case, for certain so-called {i pure} applications, it can be
    interesting to increase this number. We recommend that you read the
    {{!page:merkle}merkle-tree} tutorial to understand all the subtleties.
    
    {2 Rules}
    
    Over and above its design, Miou imposes rules to assist the programmer in
    designing his/her application. These rules are explained here. If the
    developer does not respect these rules, Miou raises an {i uncatchable}
    exception. In other words, an exception that the user has {b not} the
    definition of and cannot ignore.
    
    {3 Rule 1, wait for all your tasks.}
    
    It is forbidden to forget your children. The creation of a task necessarily
    implies that the developer waits ({!val:await}) or cancels ({!val:cancel})
    the task afterwards:
    
    {[
      # Miou.run @@ fun () -> Miou.call_cc (Fun.const ()) ;;
      Exception: Miou.Still_has_children.
    ]}
    
    {3 Rule 2, only wait for direct children.}
    
    You can only wait for your direct children. Transferring a promise to
    another task so that it can wait for it is illegal:
    
    {[
      # Miou.run @@ fun () ->
        let p = Miou.call_cc (Fun.const ()) in
        let q = Miou.call_cc (fun () -> Miou.await_exn p) in
        Miou.await_all [ p; q ] |> ignore
      Exception: Miou.Not_a_child.
    ]}
    
    {3 Rule 3, a task can only be awaited or cancelled.}
    
    Miou only allows you to wait for or cancel a task. It is also impossible to
    detach a task. For more information on this subject, we recommend reading
    the {!section:orphans} section.
    
    {3 Rule 4, a task only finishes after its children have finished.}
    
    By extension, as soon as a task is finished, all its children are finished
    too. The same applies to cancellation. If you cancel a task, you also cancel
    its children.
*)

module State = State
module Queue = Queue

module Ownership : sig
  (** {2 Ownership of resources.}

      {v La propriété, c'est le vol! v}

      Beyond the capitalist idea (even if certain libertarians would qualify the
      notion of private property), it is often useful to associate resources
      with the execution of a task in order to free them as soon as the said
      task is completed (abnormally or not).

      Miou offers such a mechanism where the user can associate a resource ['a]
      with a promise (with {!val:own}). When the task associated with this
      promise is terminated, Miou will check that all the resources have been
      released (using {!val:disown}). If this is not the case, Miou will call
      the "finaliser" specified by the user and fail with an "uncatchable"
      exception: [Resource_leak].

      Note that the user must release these resources {b and} {!val:disown}. In
      fact, {!val:disown} does {b not} call the finaliser (which is only
      executed in an abnormal situation: when the task has raised an exception
      or when a resource has not been released).
  *)

  type t
  (** The type of ownerships. *)

  val own : finally:('a -> unit) -> 'a -> t
  (** [own ~finally value] associates the value and this finaliser with the
      current promise. This way, if the current promise fails abnormally, the
      finally function will be called.

      {[
        # let show () = print_endline "Resource released"
        # Miou.run @@ fun () ->
          let p = Miou.call_cc @@ fun () ->
            let _ = Miou.Ownership.own ~finally:show () in
            failwith "p" in
          await_exn p ;;
        Resource released!
        Exception: Failure "p".
      ]}

      {b NOTE}: Finaliser can not perform OCaml's effects. This is because it is
      not "ordered" like a usual task. Using Miou functions (such as
      {!val:await} or {!val:cancel}) in the finaliser will raise an exception:
      {!exception:Effect.Unhandled}.

      It is also important to note that if a task finishes abnormally, as well
      as freeing up the resources of that task, the resources owned by the
      children will also be freed up (hence the uselessness of using await or
      cancel). *)

  val disown : t -> unit
  (** [disown t] informs Miou that you have properly released the resource. If
      the current promise ends well and the user has not [disown] the resource,
      Miou raises the uncatchable exception: [Resource_leak]

      {[
        # let show () = print_endline "Resource released" ;;
        # Miou.run @@ fun () ->
          let p = Miou.call_cc @@ fun () ->
            let _ = Miou.Ownership.own ~finally:show () in
            () in
          await_exn p ;;
        Resource released!
        Exception: Miou.Resource_leak.
      ]}

      Note that even in this situation, Miou calls the finaliser. *)

  val transfer : t -> unit
  (** [transfer t] transfers the ownership to the parent. This can be
      interesting when the resource is locked into a small promise in
      conjunction with others and the parent will make real use of it such as:

      {[
        # exception Timeout
        # Miou.await_one
            [ Miou.call_cc (fun () ->
                let socket = tcpv4 () in
                Miouu.connect socket addr;
                Ownership.transfer socket;
                socket)
            ; Miou.call @@ fun () ->
              Miouu.sleep 10.; raise Timeout ]
         |> function
         | Ok socket_connected -> ...
         | Error Timeout -> ...
      ]} *)

  val check : t -> unit
  (** [check t] verifies that the given resource [t] is owned by the current
      task. If a task tries to use a resource that does not belong to it,
      {!val:check} will raise an {i uncatchable} exception [Not_owner]. *)
end

type 'a t
(** Type of promises. *)

module Domain : sig
  module Uid : sig
    type t

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val pp : Format.formatter -> t -> unit
    val to_int : t -> int
  end

  val self : unit -> Uid.t
  val count : unit -> int
end

module Promise : sig
  module Uid : sig
    type t

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val pp : Format.formatter -> t -> unit
  end

  val pp : Format.formatter -> 'a t -> unit
  val uid : 'a t -> Uid.t
end

(** {2:orphans Daemon and orphan tasks.}

    The prerogative of absolutely awaiting all of its direct children limits the
    user to considering certain anti-patterns. The best known is the
    {i background} task: it consists of running a task that we would like to
    {i detach} from the main task so that it can continue its life in autonomy.
    For OCaml/[lwt] aficionados, this corresponds to [Lwt.async]:

    {[
      val detach : (unit -> unit t) -> unit
    ]}

    Not that we want to impose an authoritarian family approach between parent
    and children, but the fact remains that these {i orphaned} tasks have
    resources that we need to manage and free-up (even in an abnormal
    situation). We consider detachment to be an {i anti-pattern}, since it
    requires the developer to take particular care (compared to other promises)
    not to 'forget' resources that could lead to memory leaks.

    Instead of letting the developer commit to using a function that might be
    problematic, Miou offers a completely different interface that consists of
    assisting the developer in a coherent (and consistent) approach to
    responding to a particular design that is not all that systematic.

    So a promise can be associated with an {!type:orphans}. The latter will then
    collect the results of the associated promise tasks and give you back the
    promises (via {!val:care}) in a 'non-blocking' mode: applying {!val:await}
    to them will give you the results directly.

    In this way, by creating promises associated with this {!type:orphans}
    value, we can at the same time "clean up" these {i background} tasks, as
    this code shows:

    {[
      let rec clean_up orphans =
        match Miou.care orphans with
        | None -> ()
        | Some prm -> Miou.await_exn prm; clean_up orphans

      let rec server orphans =
        clean_up orphans;
        ignore (Miou.call ~orphans handler);
        server orphans

      let () = Miou.run @@ fun () -> server (Miou.orphans ())
    ]}

    There is a step-by-step {{!page:echo}tutorial} on how to create an echo
    server and how to create a {i daemon} with Miou. *)

type 'a orphans
(** The type of orphan collectors. *)

val orphans : unit -> 'a orphans
(** [orphans ()] makes a new orphan collectors which can used by {!val:call}
    and {!val:call_cc}. *)

val care : 'a orphans -> 'a t option
(** [care orphans] returns a {i ready-to-await} promise or [None]. The user
    must {i consume} the result of the promise with {!val:await}. Otherwise,
    Miou will raises the uncatchable [Still_has_children] exception. *)

(** {2 Launch a promise.} *)

val call_cc :
  ?orphans:'a orphans -> ?give:Ownership.t list -> (unit -> 'a) -> 'a t
(** [call_cc fn] (for Call with Current Continuation) returns a promise
    {!type:t} representing the state of the task given as an argument. The task
    will be carried out {b concurrently} with the other tasks. *)

val call : ?orphans:'a orphans -> ?give:Ownership.t list -> (unit -> 'a) -> 'a t
(** [call fn] returns a promise {!type:t} representing the state of the task
    given as an argument. The task will be run in parallel: the domain used to
    run the task is different from the domain with the promise. This assertion
    is always true:

    {[
      let () = Miou.run @@ fun () ->
        let p = Miou.call @@ fun () ->
          let u = Miou.Domain.self () in
          let q = Miou.call @@ fun () -> Miou.Domain.self () in
          (u, Miou.await_exn q) in
        let u, v = Miou.await_exn p in
        assert (v <> u) ;;
    ]}

    Sequential calls to {!val:call} do not guarantee that different domains are
    always chosen. This code {b may} be true.

    {[
      let () = Miou.run @@ fun () ->
        let p = Miou.call @@ fun () -> Miou.Domain.self () in
        let q = Miou.call @@ fun () -> Miou.Domain.self () in
        let u = Miou.await_exn p in
        let v = Miou.await_exn q in
        assert (u = v);
    ]}

    To ensure that tasks are properly allocated to all domains, you need to use
    {!val:parallel}.

    {b NOTE}: {!val:call} will never run a task on {i dom0} (the main domain).
    Only the other domains can manage tasks in parallel. *)

val parallel : ('a -> 'b) -> 'a list -> ('b, exn) result list
(** [parallel fn lst] is the {i fork-join} model: it is a way of setting up and
    executing parallel tasks, such that execution branches off in parallel at
    designated points in the program, to "join" (merge) at a subsequent point
    and resume sequential execution.

    Let's take the example of a sequential merge-sort:

    {[
      let sort ~compare (arr, lo, hi) =
        if hi - lo >= 2 then begin
          let mi = (lo + hi) / 2 in
          sort ~compare (arr, lo, mi);
          sort ~compare (arr, mi, hi);
          merge ~compare arr lo mi hi
        end
    ]}

    The 2 recursions work on 2 different spaces (from [lo] to [mi] and from [mi]
    to [hi]). We could parallelize their work such that:

    {[
      let sort ~compare (arr, lo, hi) =
        if hi - lo >= 2 then begin
          let mi = (lo + hi) / 2 in
          ignore (Miou.parallel (sort ~compare)
            [ (arr, lo, mi); (arr, mi, hi) ]);
          merge ~compare arr lo mi hi
        end
    ]}

    Note that {!val:parallel} launches tasks ({i fork}) and waits for them
    ({i join}). Conceptually, this corresponds to a {!val:call} on each elements
    of the given list and a {!val:await_all} on all of them, with tasks
    allocated equally to the domains.

    {b NOTE}: This function will never assign a task to {i dom0} - only the
    other domains can run tasks in parallel. *)

(** {2 System events.}

    Miou does not interact with the system, only with the OCaml runtime. As a
    result, it does not implement the usual input/output operations.
    Nevertheless, it offers a fairly simple API for using functions that
    interact with the system (and that can, above all, block).

    One of the rules of Miou is never to give it blocking functions to eat (in
    fact, it has very strict - but very simple - nutritional contraints).

    On the other hand, the system can inform you when a function is non-blocking
    (and can therefore be given to Miou). The idea is to inform Miou of the
    existence of a {i suspension point}, which it will then be {i continued}. Of
    course, it won't be able to, but as a last resort, Miou will come back to
    you to ask for a possible suspension point to continue. It will do this via
    an user's defined function, which you can specify using the {!val:run}
    function (see [events] argument).

    This user's defined function return a {!type:continue} which requires our
    {!type:syscall} (which made the suspension point) and a non-blocking
    function ([unit -> unit]). With this value, Miou will be able to continue
    from our suspension point.

    For more information on this API, a tutorial is available on how to
    implement {!page:sleepers}: tasks that block your process for a time. *)

type 'a syscall
(** The type of {i syscalls}.

    A syscall is like an unique ID of a specific suspension point made by
    {!val:suspend}. *)

type uid = private int [@@immediate]
(** The type of unique IDs of {!type:syscall}s. *)

val make : (unit -> 'a) -> 'a syscall
(** [make return] creates a {i syscall} which permits the user to create a new
    suspension point via {!val:suspend}. *)

val suspend : 'a syscall -> 'a
(** [suspend syscall] creates an user's defined suspension point. Miou will
    keep it internally and only the user is able to {i continue} it via
    {!type:events} and a {!type:continue}. *)

val uid : 'a syscall -> uid
(** [uid syscall] returns the unique ID of the syscall. *)

type continue
(** The type of continuations.

    A continuation is a suspension point and a function which can "unblock" the
    suspension point. *)

val task : 'a syscall -> (unit -> unit) -> continue
(** [task syscall fn] creates a {!type:continue} value which can be used by
    Miou to unlock via the given [fn] the user's defined suspension point
    represented by the given [syscall]. *)

type events = { select: unit -> continue list; interrupt: unit -> unit }

val is_pending : 'a syscall -> bool
(** [is_pending syscall] checks the status of the suspension point. A suspension
    point can be indirectly cancelled (if the user {!val:cancel}s the task with
    the suspension point). The user, in the [events.select] function (and
    {b only} in this function) can check the status of a suspension point. If
    {!val:is_pending} returns [true], then the suspension point still exists and
    the user should give us a function to continue, otherwise the user can
    'forget' the suspension point safely.

    {b NOTE}: this function can only be executed in the [events.select]
    function. If the user calls it elsewhere, an exception will be raised
    by Miou. *)

(** {2 Await a promise.} *)

val await : 'a t -> ('a, exn) result
(** [await prm] waits for the task associated with the promise to finish. You
    can assume that after {!val:await}, the task has ended with an exception
    with the [Error] case or normally with the [Ok] case. In the case of an
    abnormal termination (the raising of an exception), the children of the
    promise are cancelled. For instance, this code is valid:

    {[
      # Miouu.run @@ fun () ->
        let p = Miou.call_cc @@ fun () ->
          let child_of_p = Miou.call_cc @@ fun () -> Miouu.sleep 10. in
          failwith "p";
          Miou.await_exn child_of_p in
        Miou.await p ;;
      - (unit, exn) result = Error (Failure "p")
      # (* [child_of_p] was cancelled and you don't wait 10s. *)
    ]}

    Note that you should {b always} wait for your children (it's illegal to
    forget your children), as in the example above (even if an exception
    occurs). If a task does not wait for its children, an {i uncatchable}
    exception is thrown by Miou:

    {[
      # Miou.run @@ fun () ->
        ignore (Miou.call_cc (Fun.const ())) ;;
      Exception: Miou.Still_has_children.
    ]} *)

val await_exn : 'a t -> 'a
(** [await_exn prm] is an alias for {!val:await} which reraises the exception in
    the [Error] case. *)

val await_all : 'a t list -> ('a, exn) result list
(** [await_all prms] waits for all the tasks linked to the promises given. If
    one of the tasks raises an {i uncatchable} exception, {!val:await_all}
    reraises the said exception. All tasks are waited for, regardless of whether
    any fail. *)

val await_first : 'a t list -> ('a, exn) result
(** [await_first prms] waits for a task to finish (by exception or normally) and
    cancels all the others. If several tasks finish "at the same time", one of
    them is chosen randomly. This function can be useful for timeouts:

    {[
      # exception Timeout ;;
      # Miouu.run @@ fun () ->
        let p0 = Miou.call_cc (Fun.const ()) in
        let p1 = Miou.call_cc @@ fun () -> Miouu.sleep 2.; raise Timeout in
        Miou.await_first [ p0; p1 ] ;;
      - : (unit, exn) result = Ok ()
    ]} *)

val await_one : 'a t list -> ('a, exn) result
(** [await_one prms] waits for a task to finish (by exception or normally).
    Despite {!val:await_first}, {!val:await_one} does {b not} cancel all the
    others. The user must {!val:await} them otherwise Miou will not consider
    these promises as resolved and will raise [Still_has_children].

    {[
      # Miou.run @@ fun () ->
        Miou.await_one
          [ Miou.call_cc (Fun.const 1)
          ; Miou.call_cc (Fun.const 2) ] ;;
      Exception: Miou.Still_has_children
    ]} *)

val both : 'a t -> 'b t -> ('a, exn) result * ('b, exn) result
(** [both prm0 prm1] waits [prm0] {b and}) [prm1]. It's equivalent to:

    {[
      let both prm0 prm1 =
        let a = Miou.await prm0 in
        let b = Miou.await prm1 in
        (a, b)
    ]} *)

val yield : unit -> unit
(** [yield ()] reschedules tasks and give an opportunity to carry out the tasks
    that have been on hold the longest. For intance:

    {[
      # Miou.run @@ fun () ->
        let p = Miou.call_cc @@ fun () -> print_endline "Hello" in
        print_endline "World";
        Miou.await_exn p ;;
      World
      Hello
      - : unit = ()
      # Miou.run @@ fun () ->
        let p = Miou.call_cc @@ fun () -> print_endline "Hello" in
        Miou.yield ();
        print_endline "World";
        Miou.await_exn p
      Hello
      World
      - : unit = ()
    ]} *)

(** {2 Cancellation.} *)

exception Cancelled
(** Used when a task is cancelled by {!val:cancel}. *)

val cancel : 'a t -> unit
(** [cancel prm] {i asynchronously} cancels the given promise [prm]. Miou
    allows the forgetting of a cancelled promise and the forgetting of its
    children. For instance, this code is valid (despite the second one):

    {[
      # Miou.run @@ fun () ->
        ignore (Miou.cancel (Miou.call (Fun.const ()))) ;;
      - : unit = ()
      # Miou.run @@ fun () ->
        ignore (Miou.call (Fun.const ())) ;;
      Exception: Miou.Still_has_children
    ]}

    Cancellation terminates all the children. After the cancellation, the
    promise and its children all stopped. Resolved children are also cancelled
    (their results are erased). Cancelling a {i resolved} promise that has
    already been {!val:await}ed does nothing:

    {[
      # Miou.run @@ fun () ->
        let p = Miou.call_cc (Fun.const ()) in
        Miou.await_exn p;
        Miou.cancel p;
        Miou.await_exn p ;;
      - : unit = ()
    ]}

    However, cancellation does occur if a resolved promise was not awaited:

    {[
      # Miou.run @@ fun () ->
        let p = Miou.call_cc @@ fun () -> print_endline "Resolved!" in
        Miou.yield ();
        Miou.cancel p;
        Miou.await_exn p ;;
      Resolved!
      Exception: Miou.Cancelled.
    ]}

    We can only {!val:cancel} for a promise that the task has created.

    {b NOTE}: Cancellation {i asynchronicity} means that other concurrent tasks
    can run while the cancellation is in progress. In fact, in the case of an
    cancellation of a parallel task (see {!val:call}), the cancellation may take
    a certain amount of time (the time it takes for the domains to synchronise)
    which should not affect the opportunity for other concurrent tasks to run.
*)

val run :
     ?quanta:int
  -> ?events:(Domain.Uid.t -> events)
  -> ?g:Random.State.t
  -> ?domains:int
  -> (unit -> 'a)
  -> 'a
