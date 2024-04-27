(** {1 The Unix layer of Miou.}

    This module offers a re-implementation of the I/O according to Miou's
    model. It manages possible suspensions due to I/O *)

type file_descr
(** Type of file-descriptors. *)

val of_file_descr : ?non_blocking:bool -> Unix.file_descr -> file_descr
(** [of_file_descr ?non_blocking ?owner fd] creates a new {!type:file_descr}.
    Depending on [non_blocking] (defaults to [true]), we set the given [fd] to
    non-blocking mode or not. *)

val to_file_descr : file_descr -> Unix.file_descr
(** [to_file_descr fd] returns the {i real} {!type:Unix.file_descr}. *)

val tcpv4 : unit -> file_descr
(** [tcpv4 ()] allocates a new IPv4 socket. *)

val tcpv6 : unit -> file_descr
(** [tcpv6 ()] allocates a new IPv6 socket. *)

val bind_and_listen : ?backlog:int -> file_descr -> Unix.sockaddr -> unit
(** [bind_and_listen fd sockaddr] binds the given socket to the given
    [sockaddr] and set up the given [fd] for receiving connection requests.
    [backlog] is the maximal number of pending requests. *)

val accept : ?cloexec:bool -> file_descr -> file_descr * Unix.sockaddr
(** [accept ?cloexec fd] is a Miou friendly {!Unix.accept} which returns
    file descritptors in non-blocking mode. *)

val connect : file_descr -> Unix.sockaddr -> unit
(** [connect fd sockaddr] is a Miou friendly {!val:Unix.connect}. The function
    accepts only {!type:file_descr}s in non-blocking mode. *)

val read : file_descr -> bytes -> int -> int -> int
(** [read fd buf ~off ~len] reads [len] bytes from [fd] into [buf] starting at
    [off]. Return the number of bytes actually read. *)

val really_read : file_descr -> bytes -> int -> int -> unit
(** [read fd buf off len] guaranties to read [len] bytes from [fd] into
    [buf] starting at [off].

    @raise End_of_file if [fd] is closed before reaching [len] bytes read. *)

val write : file_descr -> string -> int -> int -> unit
(** [write fd str off len] writes [len] bytes starting at [off] from [str] on
    [fd]. *)

val write_string : file_descr -> string -> unit
(** [write fd str] writes [str] bytes on [fd]. *)

val close : file_descr -> unit
(** [close fd] closes properly the given [fd]. *)

val sleep : float -> unit
(** [sleep v] suspends the current task and {i sleeps} [v] seconds. *)

val run : ?g:Random.State.t -> ?domains:int -> (unit -> 'a) -> 'a

module Ownership : sig
  type file_descr

  val of_file_descr : ?non_blocking:bool -> Unix.file_descr -> file_descr
  val to_file_descr : file_descr -> Unix.file_descr
  val resource : file_descr -> Miou.Ownership.t
  val tcpv4 : unit -> file_descr
  val tcpv6 : unit -> file_descr
  val bind_and_listen : ?backlog:int -> file_descr -> Unix.sockaddr -> unit
  val accept : ?cloexec:bool -> file_descr -> file_descr * Unix.sockaddr
  val connect : file_descr -> Unix.sockaddr -> unit
  val read : file_descr -> bytes -> int -> int -> int
  val write : file_descr -> string -> int -> int -> unit
  val close : file_descr -> unit
end

(**/*)

val blocking_read : Unix.file_descr -> unit
val blocking_write : Unix.file_descr -> unit
