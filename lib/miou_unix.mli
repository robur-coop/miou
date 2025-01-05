(** {1 The Unix layer of Miou.}

    This module offers a re-implementation of the I/O according to Miou's model.
    This module is essentially concerned with reading and writing
    [Unix.file_descr] from sockets, pipes, fifos, terminals (and
    pseudo-terminals) and probably devices. For all these types of [file_descr],
    reading/writing can block (waiting for bytes or waiting for the system's
    internal buffer to be free).

    This is not {i generally} the case for files and folders where
    [read]/[write] does not block. So, as far as files/folders are concerned,
    there is no point in using the suspend/resume mechanisms offered by Miou.
    These mechanisms (and the use of [Unix.select]) could actually degrade
    performance. It is therefore advisable to use the [Unix] module directly
    rather than [Miou_unix] for files/folders.

    It should be noted, however, that reading/writing files can take a long
    time. So, in a cooperative context (with [Miou.async]), it may be worthwhile
    to increase the availability of other tasks to run with [Miou.yield] - so
    associate a [Miou.yield] with these operations. However, in the case of
    parallelization ([Miou.call]), it is not necessary to cooperate with the
    other tasks as they run in parallel. In this sense, and depending on the
    design of your application, these operations should, or should not, be
    associated with a [Miou.yield]. *)

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

val bind_and_listen :
     ?backlog:int
  -> ?reuseaddr:bool
  -> ?reuseport:bool
  -> file_descr
  -> Unix.sockaddr
  -> unit
(** [bind_and_listen fd sockaddr] binds the given socket to the given [sockaddr]
    and set up the given [fd] for receiving connection requests. [backlog] is
    the maximal number of pending requests. The optional argument [reuseaddr]
    (defaults to [true]) sets the [REUSEADDR] socket option on the given [fd].
    The optional argument [reuseport] (defaults to [true] sets the [REUSEPORT]
    socket option on the given [fd]. *)

val accept : ?cloexec:bool -> file_descr -> file_descr * Unix.sockaddr
(** [accept ?cloexec fd] is a Miou friendly {!Unix.accept} which returns file
    descritptors in non-blocking mode. *)

val connect : file_descr -> Unix.sockaddr -> unit
(** [connect fd sockaddr] is a Miou friendly {!val:Unix.connect}. The function
    accepts only {!type:file_descr}s in non-blocking mode. *)

val read : file_descr -> ?off:int -> ?len:int -> bytes -> int
(** [read fd buf ~off ~len] reads up to [len] bytes (defaults to
    [Bytes.length buf - off] from the given file-descriptor [fd], storing them
    in byte sequence [buf], starting at position [off] in [buf] (defaults to
    [0]). It returns the actual number of characters read, between 0 and [len]
    (inclusive).

    @raise Unix_error
      raised by the system call {!val:Unix.read}. The function handles
      {!val:Unix.EINTR}, {!val:Unix.EAGAIN} and {!val:Unix.EWOULDBLOCK}
      exceptions and redo the system call.

    @raise Invalid_argument
      if [off] and [len] do not designate a valid range of [buf] *)

val really_read : file_descr -> ?off:int -> ?len:int -> bytes -> unit
(** [really_read fd buf ~off ~len] reads [len] bytes (defaults to
    [Bytes.length buf - off]) from the given file-descriptor [fd], storing them
    in byte sequence [buf], starting at position [off] in [buf] (defaults to
    [0]). If [len = 0], [really_read] does nothing.

    @raise Unix_error
      raised by the system call {!val:Unix.read}. The function handles
      {!val:Unix.EINTR}, {!val:Unix.EAGAIN} and {!val:Unix.EWOULDBLOCK}
      exceptions and redo the system call.

    @raise End_of_file
      if {!val:Unix.read} returns [0] before [len] characters have been read.

    @raise Invalid_argument
      if [off] and [len] do not designate a valid range of [buf] *)

val write : file_descr -> ?off:int -> ?len:int -> string -> unit
(** [write fd str ~off ~len] writes [len] bytes (defaults to
    [String.length str - off]) from byte sequence [buf], starting at offset
    [off] (defaults to [0]), to the given file-descriptor [fd].

    @raise Unix_error
      raised by the system call {!val:Unix.read}. The function handles
      {!val:Unix.EINTR}, {!val:Unix.EAGAIN} and {!val:Unix.EWOULDBLOCK}
      exceptions and redo the system call.

    @raise Invalid_argument
      if [off] and [len] do not designate a valid range of [buf] *)

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
  val read : file_descr -> ?off:int -> ?len:int -> bytes -> int
  val really_read : file_descr -> ?off:int -> ?len:int -> bytes -> unit
  val write : file_descr -> ?off:int -> ?len:int -> string -> unit
  val close : file_descr -> unit
end

(**/*)

val blocking_read : Unix.file_descr -> unit
val blocking_write : Unix.file_descr -> unit
