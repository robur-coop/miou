module Tq : sig
  type 'a t

  exception Empty

  val enqueue : 'a t -> 'a -> unit
  val dequeue : 'a t -> 'a
  val make : unit -> 'a t
  val is_empty : 'a t -> bool
end

module Promise : sig
  type 'a state =
    | Pending  (** The task is not yet resolved. *)
    | Resolved of 'a  (** The normal termination. *)
    | Failed of exn  (** Abnormal termination. *)

  exception Cancelled

  type 'a t
  (** Type of promises. *)

  (** {2 Launch a promise.} *)

  val call_cc : (unit -> 'a) -> 'a t
  (** [call_cc fn] (for Call with Current Continuation) returns a promise which
      will be executed {b cooperatively} with other promises. *)

  val call : (unit -> 'a) -> 'a t
  (** [call fn] returns a promise which will be executed {b in parallel} with
      other promises. *)

  val sleep : int64 -> unit t

  val yield : unit -> unit
  (** Suspends and schedules the current task, this gives other promises of the
      same {!type:Uid.t} a chance to run, useful to be called in cpu-intensive
      paths. *)

  val cancel : 'a t -> unit

  (** {2 Await a promise.} *)

  val await : 'a t -> ('a, exn) result
  val await_exn : 'a t -> 'a
  val await_first : 'a t list -> ('a, exn) result
  val state : 'a t -> 'a state
end

val run : ?g:Random.State.t -> (unit -> 'a) -> 'a
