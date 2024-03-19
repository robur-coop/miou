module Heapq = Heapq
module Logs = Logs
module Fmt = Fmt
module Trigger = Sync.Trigger
module Computation = Sync.Computation
module Queue = Queue

module Domain : sig
  module Uid : sig
    type t [@@immediate]

    val of_int : int -> t
    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
  end

  val self : unit -> Uid.t
end

type 'a t

module Promise : sig
  type nonrec 'a t = 'a t

  module Uid : sig
    type t [@@immediate]

    val pp : Format.formatter -> t -> unit
  end

  val uid : 'a t -> Uid.t
end

exception Cancelled
exception No_domain_available

(** {1: ...} *)

type 'a orphans

val orphans : unit -> 'a orphans
val care : 'a orphans -> 'a t option option

(** {1: ...} *)

val call_cc : ?orphans:'a orphans -> (unit -> 'a) -> 'a t
val call : ?orphans:'a orphans -> (unit -> 'a) -> 'a t
val parallel : ('a -> 'b) -> 'a list -> ('b, exn) result list

(** {1: ...} *)

val await : 'a t -> ('a, exn) result
val await_exn : 'a t -> 'a
val await_one : 'a t list -> ('a, exn) result
val await_first : 'a t list -> ('a, exn) result
val await_all : 'a t list -> ('a, exn) result list

(** {1: ...} *)

val cancel : 'a t -> unit

(** {1: ...} *)

val yield : unit -> unit
val stats : unit -> unit

(** {1: ...} *)

type syscall
type signal
type uid = private int [@@immediate]

val syscall : unit -> syscall
val suspend : syscall -> unit
val signal : syscall -> signal
val uid : syscall -> uid

type select = block:bool -> uid list -> signal list
type events = { select: select; interrupt: unit -> unit }

val run :
     ?quanta:int
  -> ?g:Random.State.t
  -> ?domains:int
  -> ?events:(Domain.Uid.t -> events)
  -> (unit -> 'a)
  -> 'a

module Mutex : sig
  type t

  val create : unit -> t
  val unlock : t -> unit
  val lock : t -> unit
  val try_lock : t -> bool

  val protect : t -> (unit -> 'a) -> 'a
  (** [protect t fn] runs [fn] in a critical section where [t] is locked
      ({i atomically}); it then takes care of releasing [t] whether [fn]
      returned a value or raised an exception.

      The unlocking operation is guaranteed to always takes place, even in the
      event a cancellation is ordered by the parent. *)
end

module Condition : sig
  type t

  val create : unit -> t
  val broadcast : t -> unit
  val signal : t -> unit
  val wait : t -> Mutex.t -> unit
end
