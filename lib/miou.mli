module Queue : sig
  (** A lock-free queue.

      To be able to implement a scheduler across multiple domains, we must
      have a Thread-safe Queue. This thread-safe implementation provides basic
      operations for a queue: {!val:enqueue} & {!val:dequeue}. *)

  type 'a t
  (** Type of lock-free queues. *)

  exception Empty

  val enqueue : 'a t -> 'a -> unit
  val dequeue : 'a t -> 'a
  val make : unit -> 'a t
  val is_empty : 'a t -> bool
  val transfer : 'a t -> 'a t
end

module Domain_id : sig
  (** An unique identifier for domains. *)

  type t
  (** The type of identifiers. *)

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit

  val self : unit -> t
  (** [self ()] returns the identifier of the current domain that runs you. *)
end

module Id : sig
  (** A unique identifier for promises. *)

  type t
  (** The type of identifiers. *)

  val null : t
  (** [null] is an {i impossible} value of {!type:t}. Actually, {!type:t} is
      used to identify {!type:t} and they will {b never} have such
      value as their identifiers. *)

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module Ownership : sig
  (** A capitalism idea into our scheduler.

      [miou] offers a mechanism for protecting resources if a task finishes
      abnormally (with an exception). In such a situation, the [finally]
      function associated with the resource will be executed. An example can be
      applied to file-descriptors and associate [Unix.close] with them as a
      "finaliser":

      {[
        let fd = Unix.socket ... in
        let rs = Ownership.make ~finally:Unix.close fd in
        Ownership.own rs
      ]} *)

  type t
  (** The type of ownerships. *)

  val own : finally:('a -> unit) -> 'a -> t
  (** [own ~finally value] associates the value and this finaliser with the
      current promise. This way, if the current promise fails abnormally, the
      finally function will be called.

      {[
        # let show () = print_endline "Resource released"
        # Miou.(run @@ fun () ->
          let p = call_cc @@ fun () ->
            let _ = Ownership.own ~finally:show () in
            failwith "p" in
          await_exn p) ;;
        Resource released!
        Exception: Failure "p".
      ]} *)

  val disown : t -> unit
  (** [disown t] informs [miou] that you have properly released the resource. If
      the current promise ends well and the user has not [disown] the resource,
      [miou] raises the exception: [Resource_leak]:

      {[
        # let show () = print_endline "Resource released"
        # Miou.(run @@ fun () ->
          let p = call_cc @@ fun () ->
            let _ = Ownership.own ~finally:show () in
            () in
          await_exn p) ;;
        Resource released!
        Exception: Miou.Resource_leak.
      ]}

      Note that even in this situation, [miou] calls the finaliser. *)

  val transfer : t -> unit
  (** [transfer t] transfers the ownership to the parent. This can be
      interesting when the resource is locked into a small promise in
      conjunction with others and the parent will make real use of it such as:

      {[
        Miou.await_first
          [ Miou.call_cc @@ fun () ->
            let socket = tcpv4 () in
            Miouu.connect socket addr;
            Ownership.transmit socket;
            socket
          ; Miou.call_cc @@ fun () -> sleep 10.; raise Timeout ]
      ]} *)

  val check : t -> unit
  (** [check t] verifies that the given resource [t] is owned by the current
      promise. *)
end

(** {1 A promise for a better futur.}

    A {b promise} is a function ([unit -> 'a]) that will be executed by the
    scheduler in the near future. The user can launch a promise (and notify by
    this way the scheduler of a new task to do) as well as wait for the result
    of a promise (see {!val:await}).

    A promise can be executed concurrently with other promises (see
    {!val:call_cc}) or in parallel with other promises (see {!val:call}).
*)

type 'a t
(** Type of promises. *)

val pp : Format.formatter -> 'a t -> unit
(** A simple pretty-printer of a promise which shows you the domain where the
    promise run and its unique ID. *)

(** {2 Daemon and orphan tasks.}

    The prerogative of absolutely expecting all of its direct children limits
    the user to considering certain anti-patterns. The best known is the
    {i background} task: this consists of running a task that we would like to
    'detach' from the main task so that it can continue its life in autonomy.

    Not that we want to impose an authoritarian family approach, but the fact
    remains that these {i orphaned} tasks have resources that we need to manage
    and free up (even in an abnormal situation). And we'd like to sleep easy
    tonight.

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

      let server orphans =
        clean_up orphans;
        ignore (Miou.call ~orphans handler);
        server orphans in
      server (Miou.oprhans ())
    ]} *)

type 'a orphans
(** The type of orphan collectors. *)

val orphans : unit -> 'a orphans
(** [orphans ()] makes a new orphan collectors which can be used by
    {!val:call} and {!val:call_cc}. *)

val care : 'a orphans -> 'a t option
(** [care orphans] returns a {i ready-to-await} promise or [None]. The user must
    {i consume} the result of the promise with {!val:await}. Otherwise,
    [miou] will raises [Still_has_children]. *)

(** {2 Launch a promise.} *)

val call_cc :
  ?orphans:'a orphans -> ?give:Ownership.t list -> (unit -> 'a) -> 'a t
(** [call_cc fn] (for Call with Current Continuation) returns a promise which
    will be executed {b cooperatively} with other promises. *)

val call : ?orphans:'a orphans -> ?give:Ownership.t list -> (unit -> 'a) -> 'a t
(** [call fn] returns a promise which will be executed {b in parallel} with
    other promises. [miou] pre-allocates domains that are waiting to perform
    this kind of task. The user does {b not} choose the domain on which the task
    will be executed. [miou] {b randomly} chooses which of the domains will
    perform the task. *)

(* {2 Cancellation.} *)

exception Cancelled

val cancel : 'a t -> unit
(** [cancel prm] asynchronously tries to cancel the given promise [prm]. [miou]
    allows the forgetting of a cancelled promise and the forgetting of its
    children. For instance, this code is valid (despite the second one):

    {[
      # Miou.(run @@ fun () ->
              ignore (cancel (call (Fun.const ())))) ;;
      - : unit = ()
      # Miou.(run @@ fun () ->
              ignore (call (Fun.const ()))) ;;
      Exception: Miou.Still_has_children
    ]}

    Cancellation will try to finish all the children and will wait until all the
    children are finished (once again, termination intervenes if the promise has
    been cancelled {b or} resolved). You can sleep soundly after the
    cancellation, and the promise that all its children have stopped. *)

(** {2 Await a promise.} *)

val await : 'a t -> ('a, exn) result
val await_exn : 'a t -> 'a
val await_one : 'a t list -> ('a, exn) result
val await_all : 'a t list -> ('a, exn) result list
val await_first : 'a t list -> ('a, exn) result

(** {2 State introspection.} *)

type 'a state =
  | Resolved of 'a  (** Normal termination. *)
  | Failed of exn  (** Abnormal termination. *)
  | Pending  (** Not yet resolved. *)

val state : 'a t -> 'a state
(** [state prm] returns the current state of the given promise {!type:t}
      [prm]. *)

(** {2 Syscalls.}

    [miou] does not interact with the system, only with the OCaml runtime. As a
    result, it does not implement the usual input/output operations.
    Nevertheless, it offers a fairly simple API for using functions that
    interact with the system (and that can, above all, block).

    One of the rules of [miou] is never to give it blocking functions to eat (in
    fact, it has very strict - but very simple - nutritional constraints).

    On the other hand, the system can inform you when a function is non-blocking
    (and can therefore be given to [miou]). The idea is to inform [miou] of the
    existence of a {i promise}, which it will then try to resolve. Of course, it
    won't be able to, but as a last resort, [miou] will come back to you to ask
    for a possible task to resolve this promise. It will do this via an user's
    defined function, which you can specify using the {!val:run} function (see
    [events] argument).

    This user's defined function return a {!type:task} which is a promise
    associated with a {b non-blocking} task ([unit -> unit]) that would resolve
    it. At last, [miou] will be able to fulfil your promise!

    For more information on this API, a tutorial is available on how to
    implement {!page:sleepers}: tasks that block your process for a time.
*)

type 'a syscall
(** The type of {i syscalls}.

    A syscall is like a promise (see {!type:t}), but the user can only
    {!val:suspend} the execution flow with it. *)

val make : ?give:Ownership.t list -> (unit -> 'a) -> 'a syscall
(** [make return] creates a {i promise} that will {b never} be resolved. For
    the example, this code fails:

    {[
      # Miou.(run @@ fun () -> let v = make (Fun.const ()) in
              suspend v) ;;
      Exception: Miou.Still_has_children
    ]}

    However, if you keep this promise somewhere and specify a "select" function
    that proposes a task to resolve it, the program should then terminate:

    {[
      # let global = ref None ;;
      # let select () = match !global with
        | Some p -> [ Miou.task p (fun () -> global := None) ]
        | None -> []
        ;;
      # let events = { Miou.select; Miou.interrupt= ignore } ;;
      # Miou.(run ~events @@ fun () ->
        let v = make (Fun.const ()) in
        global := Some v; suspend v) ;;
      - : (unit, exn) result = Ok ()
    ]}

    As you can see, the use of {!val:make} is very intrinsic to the creation of
    the [events] function. *)

val suspend : 'a syscall -> ('a, exn) result
(** [suspend syscall] suspends the execution flow and will be resumed when the
    user gives a {b non-blocking} function (a {!type:task}) via {!type:events}
    that resolves the syscall. *)

val is_pending : 'a syscall -> bool
(** [is_pending syscall] returns [true] if the given [syscall] is not yet
    resolved (or cancelled). This function can be useful to {i clean-up}
    syscalls that have been cancelled by [miou]. *)

val uid : 'a syscall -> Id.t
(** [uid syscall] returns a unique identifier of the promise. *)

val yield : unit -> unit
(** [yield ()] reschedules tasks and give a chance to all of them to be executed
    then. For instance:

    {[
      # Miou.run @@ fun () ->
        let p = Miou.call_cc (fun () -> print_endline "Hello") in
        print_endline "World";
        Miou.await_exn prm ;;
      World
      Hello
      - : unit = ()
      # Miou.run @@ fun () ->
        let p = Miou.call_cc (fun () -> print_endline "Hello") in
        yield ();
        print_endline "World";
        Miou.await_exn prm ;;
      Hello
      World
      - : unit = ()
    ]} *)

type task
(** Type of tasks. *)

type events = { interrupt: unit -> unit; select: unit -> task list }

val task : 'a syscall -> (unit -> unit) -> task
(** [task prm fn] creates a new task associated with a {!type:syscall}
    created by the user. The task must be a non-blocking function to resolve the
    associated promise. *)

val run :
     ?g:Random.State.t
  -> ?domains:int
  -> ?events:(Domain_id.t -> events)
  -> (unit -> 'a)
  -> 'a
