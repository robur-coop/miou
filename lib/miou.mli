module Heapq = Heapq
module Logs = Logs
module Fmt = Fmt
module Trigger = Sync.Trigger
module Computation = Sync.Computation

module Domain : sig
  module Uid : sig
    type t [@@immediate]

    val of_int : int -> t
    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val gen : unit -> t
  end

  val self : unit -> Uid.t
end

type 'a t

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
end

module Condition : sig
  type t

  val create : unit -> t
  val broadcast : t -> unit
  val signal : t -> unit
  val wait : t -> Mutex.t -> unit
end
