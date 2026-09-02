(** A direct binding of [epoll(7)]. *)

type t
(** An epoll instance. *)

module Flags : sig
  type t = private int

  val epollin : t
  val epollout : t
  val epollerr : t
  val epollhup : t
  val epollrdhup : t
  val epolloneshot : t
  val empty : t
  val ( + ) : t -> t -> t
  val mem : t -> t -> bool
  val of_int : int -> t
end

val invalid_fd : t
val create : unit -> t
val close : t -> unit

val add : t -> Unix.file_descr -> Flags.t -> int
(** [add t fd flags] registers [fd]. Returns [0] on success, otherwise the
    [errno] (registering a descriptor that is already there gives [EEXIST]). *)

val upd : t -> Unix.file_descr -> Flags.t -> int
(** [upd t fd flags] changes (or re-arms) the events watched for [fd]. Returns
    [0] on success, otherwise the [errno] (typically [ENOENT] if [fd] was closed
    in the meantime, which drops it from the set automatically). *)

val del : t -> Unix.file_descr -> int
(** [del t fd] stops watching [fd]. *)

type events
(** A buffer holding the descriptors reported ready by the last {!val:wait}. *)

val events : int -> events

type epoll_timeout = Infinite | No_wait | Nanoseconds of int64

val wait : t -> events -> epoll_timeout -> int
(** [wait t events timeout] fills [events] and returns how many descriptors are
    ready. An interruption by a signal returns [0]. *)

val iter : events -> int -> (Unix.file_descr -> Flags.t -> unit) -> unit
(** [iter events n fn] applies [fn] to the [n] descriptors reported ready. *)
