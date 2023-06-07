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
  type t

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

  type 'a t
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

  (** {2 Await a promise.} *)

  val await : 'a t -> ('a, exn) result
  val await_exn : 'a t -> 'a
  val await_first : 'a t list -> ('a, exn) result
  val await_first_exn : 'a t list -> 'a

  (** {2 State of a promise.} *)

  type 'a state =
    | Pending  (** The task is not yet resolved. *)
    | Resolved of 'a  (** The normal termination. *)
    | Failed of exn  (** Abnormal termination. *)

  val state : 'a t -> 'a state
end

val run : ?g:Random.State.t -> (unit -> 'a) -> 'a
