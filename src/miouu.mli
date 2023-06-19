val sleep : float -> (unit, exn) result
(** [sleep n] sleeps [n] second(s). *)

type file_descr
(** Type of file descriptors. *)

val read : file_descr -> bytes -> off:int -> len:int -> int
(** [read fd buf ~off ~len] tries to read [len] bytes from the given [fd] into
    [buf] starting at [off]. [read] {b takes} the ownership on [buf] - don't
    reuse it into another task until this one is resolved! Returns the number of
    bytes we were able to read. *)

val write : file_descr -> string -> off:int -> len:int -> unit
(** [write fd str ~off ~len] tries to {b fully} write [len] bytes starting at
    [off] from [str] into the given [fd] . *)

val accept :
  ?cloexec:bool -> file_descr -> Miou.Own.t * file_descr * Unix.sockaddr
(** [accept ?cloexec fd] is a [miou] friendly {!val:Unix.accept} which returns a
    new {!type:file_descr} (in a non-blocking mode). It also returns the address
    to which the socket is linked and an {!module:Miou.Own} value (to close the
    socket if the current task ends abnormally). *)

val connect : file_descr -> Unix.sockaddr -> unit
(** [connect fd sockaddr] is a [miou] friendly {!val:Unix.connect}. The given
    [fd] must be in a non-blocking mode. *)

val of_file_descr : ?non_blocking:bool -> Unix.file_descr -> file_descr
(** [of_file_descr ?non_blocking fd] returns a {!type:file_descr} and set it to
    the non-blocking mode if [non_blocking = true] (defaults to [true]). *)

val to_file_descr : file_descr -> Unix.file_descr
(** Returns the {!type:Unix.file_descr}. *)

(** {2 The Miou's entry point.} *)

val run : ?g:Random.State.t -> (unit -> 'a) -> 'a
