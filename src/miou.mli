module Tq = Tq

module Did : sig
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val concurrent : unit -> t
  val parallel : unit -> t
end

module Id : sig
  type t

  val null : t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module Prm : sig
  type 'a t

  val pp : Format.formatter -> 'a t -> unit
  val await : 'a t -> ('a, exn) result
  val await_exn : 'a t -> 'a
  val await_first : 'a t list -> ('a, exn) result
  val await_first_exn : 'a t list -> 'a
  val await_all : 'a t list -> ('a, exn) result list
  val await_all_ign : 'a t list -> unit
  val call_cc : (unit -> 'a) -> 'a t
  val call : (unit -> 'a) -> 'a t
  val make : return:(unit -> 'a) -> 'a t
  val uid : 'a t -> Id.t

  exception Cancelled

  val failed_with : 'a t -> exn -> unit
  val cancel : 'a t -> unit
end

val yield : unit -> unit

type syscall

val syscall : 'a Prm.t -> (unit -> unit) -> syscall

val run :
     ?g:Random.State.t
  -> ?events:(unit -> syscall list option)
  -> (unit -> 'a)
  -> 'a
