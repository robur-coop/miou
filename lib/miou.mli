module Queue : sig
  (** A lock-free queue.

      To be able to implement a scheduler across multiple domains, we must
      have a domain-safe Queue. This domain-safe implementation provides basic
      operations for a queue: {!val:enqueue} & {!val:dequeue}. *)

  type 'a t
  (** Type of lock-free queues. *)

  exception Empty
  (** Raised when {!dequeue} is applied to an empty queue. *)

  val enqueue : 'a t -> 'a -> unit
  (** [enqueue t x] adds the element [x] at the end of the queue [t]. *)

  val dequeue : 'a t -> 'a
  (** [dequeue t] removes and returns the first element in the queue [t],
      or raise {!exception:Empty} if the queue is empty. *)

  val create : unit -> 'a t
  (** [create ()] returns a new queue, initially empty. *)

  val is_empty : 'a t -> bool
  (** [is_empty] returns [true] if the given queue is empty, it returns [false]
      otherwise. *)

  val transfer : 'a t -> 'a t
  (** [transfer q] returns a new queue which contains all of [q]'s elements,
      then clears [q]. *)

  val length : 'a t -> int
  (** Return the number of elements in a queue. *)

  val to_list : 'a t -> 'a list
  (** Return a list representation of the given queue. *)
end

module Ownership : sig
  (** {2 Ownership of resources.}

      {v La propriété, c'est le vol v}

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
      }]

      {b NOTE}: Finaliser has no effect. This is because it is not "ordered"
      like a usual task. Using Miou functions (such as {!val:await} or
      {!val:cancel}) in the finaliser will raise an exception:
      {!exn:Stdlib.Effect.Unhandled}. *)

  val disown : t -> unit
  (** [disown t] informs [miou] that you have properly released the resource. If
      the current promise ends well and the user has not [disown] the resource,
      [miou] raises the uncatchable exception: [Resource_leak]

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

      Note that even in this situation, [miou] calls the finaliser. *)

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
end

module Promise : sig
  module Uid : sig
    type t

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val pp : Format.formatter -> t -> unit
  end

  val pp : Format.formatter -> 'a t -> unit
end

(** {2 Daemon and orphan tasks.}

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
    server and how to create a {i daemon} with [miou]. *)

type 'a orphans
(** The type of orphan collectors. *)

val orphans : unit -> 'a orphans
(** [orphans ()] makes a new orphan collectors which can used by {!val:call}
    and {!val:call_cc}. *)

val care : 'a orphans -> 'a t option
(** [care orphans] returns a {i ready-to-await} promise or [None]. The user
    must {i consume} the result of the promise with {!val:await}. Otherwise,
    [miou] will raises the uncatchable [Still_has_children] exception. *)

(** {2 Launch a promise.} *)

val call_cc :
  ?orphans:'a orphans -> ?give:Ownership.t list -> (unit -> 'a) -> 'a t
(** [call_cc fn] (for Call with Current Continuation) returns a promise
    {!type:t} representing the state of the task given as an argument. The task
    will be carried out {b cooperatively} with the other tasks. *)

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
    {!val:parallel}. *)

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
    allocated equally to the domains. *)

(** {2 System events.}

    [miou] does not interact with the system, only with the OCaml runtime. As a
    result, it does not implement the usual input/output operations.
    Nevertheless, it offers a fairly simple API for using functions that
    interact with the system (and that can, above all, block).

    One of the rules of [miou] is never to give it blocking functions to eat (in
    fact, it has very strict - but very simple - nutritional contraints).

    On the other hand, the system can inform you when a function is non-blocking
    (and can therefore be given to [miou]). The idea is to inform [miou] of the
    existence of a {i suspension point}, which it will then be {i continued}. Of
    course, it won't be able to, but as a last resort, [miou] will come back to
    you to ask for a possible suspension point to continue. It will do this via
    an user's defined function, which you can specify using the {!val:run}
    function (see [events] argument).

    This user's defined function return a {!type:continue} which requires our
    {!type:syscall} (which made the suspension point) and a non-blocking
    function ([unit -> unit]). With this value, [miou] will be able to continue
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
(** [suspend syscall] creates an user's defined suspension point. [miou] will
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
    [miou] to unlock via the given [fn] the user's defined suspension point
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
    by [miou]. *)

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
    others. The user must {!val:await} them otherwise [miou] will not consider
    these promises as resolved and will raise [Still_has_children].

    {[
      # Miou.run @@ fun () ->
        Miou.await_one
          [ Miou.call_cc (Fun.const 1)
          ; Miou.call_cc (Fun.const 2) ] ;;
      Exception: Miou.Still_has_children
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
(** [cancel prm] {i asynchronously} cancels the given promise [prm]. [miou]
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
