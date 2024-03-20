type file_descr

val of_file_descr : ?non_blocking:bool -> Unix.file_descr -> file_descr
val to_file_descr : file_descr -> Unix.file_descr

(** {1 ...} *)

val tcpv4 : unit -> file_descr
val tcpv6 : unit -> file_descr
val bind_and_listen : ?backlog:int -> file_descr -> Unix.sockaddr -> unit
val accept : ?cloexec:bool -> file_descr -> file_descr * Unix.sockaddr
val connect : file_descr -> Unix.sockaddr -> unit
val read : file_descr -> bytes -> int -> int -> int
val write : file_descr -> string -> int -> int -> unit
val close : file_descr -> unit

(** {1 ...} *)

val sleep : float -> unit

(** {1 ...} *)

val run : ?g:Random.State.t -> ?domains:int -> (unit -> 'a) -> 'a

(** {1 ...} *)

val blocking_read : Unix.file_descr -> unit
val blocking_write : Unix.file_descr -> unit

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
