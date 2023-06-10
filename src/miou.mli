module Tq : sig
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
end

module Id : sig
  (** A unique identifier for promises. *)

  type t
  (** The type of identifiers. *)

  val null : t
  (** [null] is an {i impossible} value of {!type:t}. Actually, {!type:t} is
      used to identify {!type:Prm.t} and they will {b never} have such
      value as their identifiers. *)

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end

module Prm : sig
  (** A promise for a better futur.

      A {b promise} is a function ([unit -> 'a]) that will be executed by the
      scheduler in the near future. The user can launch a promise (and notify
      by this way the scheduler of a new task to do) as well as wait for the
      result of a promise (see {!val:await}).

      A promise can be executed concurrently with other promises (see
      {!val:call_cc}) or in parallel with other promises (see {!val:call}).
  *)

  type !+'a t
  (** Type of promises. *)

  (** {2 Launch a promise.} *)

  val call_cc : (unit -> 'a) -> 'a t
  (** [call_cc fn] (for Call with Current Continuation) returns a promise which
      will be executed {b cooperatively} with other promises. *)

  val call : (unit -> 'a) -> 'a t
  (** [call fn] returns a promise which will be executed {b in parallel} with
      other promises. *)

  val yield : unit -> unit
  (** Suspends and schedules the current task, this gives other promises of the
      same {!type:Uid.t} a chance to run, useful to be called in cpu-intensive
      paths. *)

  (** {2 Cancellation.} *)

  exception Cancelled

  val cancel : 'a t -> unit
  (** Cancellation allows the parent to kill a child using the associated
      promise. Cancellation marks the promise as {i consumed}, which means that
      you can forget about the promise, and [miou] will not be informed that the
      child is still alive. For instance, this is a valid code:

      {[
        # Miou.(run @@ fun () -> Prm.cancel (Prm.call (Fun.const 1))) ;;
        - : unit = ()
      ]}

      We simply consider that cancellation is equivalent to looking into the
      promise (in other words, by using [cancel], you know that at worst, the
      state of the promise is [Failed Cancelled]). It may happen that you wish
      to cancel a promise that has already been resolved; in this case, no state
      transition are made.

      If you cancel a task, all the children in that task will be cancelled too.
      Be careful, though, as cancelling them requires you to somehow observe the
      transition of state of these children. If you follow Miou's rules, you
      should be looking after your children anyway. *)

  (** {2 Await a promise.} *)

  val await : 'a t -> ('a, exn) result
  val await_exn : 'a t -> 'a
  val await_first : 'a t list -> ('a, exn) result
  val await_first_exn : 'a t list -> 'a
  val await_one : 'a t list -> ('a, exn) result
  val await_one_exn : 'a t list -> 'a
  val await_all_ign : 'a t list -> unit
  val await_all_exn : 'a t list -> (unit, exn) result

  (** {2 State of a promise.} *)

  type !+'a state =
    | Pending  (** The task is not yet resolved. *)
    | Resolved of 'a  (** The normal termination. *)
    | Failed of exn  (** Abnormal termination. *)

  val state : 'a t -> 'a state
end

module Sysc : sig
  (** Syscalls

      [miou] does not interact with the system, only with the OCaml runtime. As
      a result, it does not implement the usual input/output operations.
      Nevertheless, it offers a fairly simple API for using functions that
      interact with the system (and that can, above all, block).

      One of the rules of [miou] is never to give him blocking functions to eat
      (in fact, he has very strict - but very simple - nutritional constraints).

      On the other hand, the system can inform you when a function is
      non-blocking (and can therefore be given to [miou]). The idea is to inform
      [miou] of the existence of a {i promise}, which it will then try to
      resolve. Of course, it won't be able to, but as a last resort, [miou] will
      come back to you to ask for a possible task to resolve this promise. It
      will do this via an user's defined function, which you can specify using
      the {!val:run} function (see [events] argument).

      This user's defined function return a {!type:syscall} which is a promise
      associated with a {b non-blocking} task ([unit -> 'a]) that would resolve
      it. At last, [miou] will be able to fulfil your promise!

      For more information on this API, a tutorial is available on how to
      implement {i sleepers}: tasks that block your process for a time.
   *)

  type !-'a t
  (** Type of syscalls. *)

  val make : unit -> 'a t
  (** [make ()] creates a {i promise} (something like {!type:Prm.t}) that
      will {b never} be resolved. For the example, this code does not terminate:

      {[
        # Miou.(run @@ fun () -> let v = Sysc.make () in Sysc.await v) ;;
      ]}

      However, if you keep this promise somewhere and specify an "events"
      function that proposes a task to resolve it, the program should
      then terminate:

      {[
        # let global = ref None ;;
        # let events () = match !global with
          | Some prm -> Some [ Miou.syscall prm (Fun.const ()) ]
          | None -> None
          ;;
        # Miou.(run ~events @@ fun () ->
          let v = Sysc.make () in
          global := Some v; Sysc.await v)
        - : (unit, exn) result = Ok ()
      ]}

      As you can see, the use of {!val:make} is very intrinsic to the creation
      of the [events] function. *)

  val await : 'a t -> ('a, exn) result
  (** [await syscall] waits for the promise to be resolved and returns its
      value. *)

  val uid : 'a t -> Id.t
  (** [uid syscall] returns a unique identifier of the promise. *)
end

type syscall

val syscall : 'a Sysc.t -> (unit -> 'a) -> syscall

val run :
     ?g:Random.State.t
  -> ?events:(unit -> syscall list option)
  -> (unit -> 'a)
  -> 'a
