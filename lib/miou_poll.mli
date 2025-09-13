(** A direct binding of [poll(2)]. *)

type t

val create : ?maxfds:int -> unit -> t
val maxfds : t -> int

module Flags : sig
  type t

  val pollin : t
  val pollpri : t
  val pollout : t
  val pollerr : t
  val pollhup : t
  val pollnval : t
  val empty : t
  val ( + ) : t -> t -> t
  val mem : t -> t -> bool
  val to_int : t -> int
  val of_int : int -> t
end

val has_ppoll : bool
val invalid_fd : Unix.file_descr

type poll_timeout = Infinite | No_wait | Milliseconds of int

val poll : t -> int -> poll_timeout -> int

type ppoll_timeout = Infinite | No_wait | Nanoseconds of int64

val ppoll : t -> int -> ppoll_timeout -> int list -> int
val ppoll_or_poll : t -> int -> ppoll_timeout -> int
val set_index : t -> int -> Unix.file_descr -> Flags.t -> unit
val invalidate_index : t -> int -> unit
val get_revents : t -> int -> Flags.t
val get_fd : t -> int -> Unix.file_descr
val iter : t -> int -> (int -> Unix.file_descr -> Flags.t -> unit) -> unit
