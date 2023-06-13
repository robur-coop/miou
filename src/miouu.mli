val sleep : float -> (unit, exn) result
(** [sleep n] sleeps [n] second(s). *)

type file_descr
(** Type of file descriptors. *)

(*
val read : file_descr -> bytes -> off:int -> len:int -> (int, exn) result
(** [read fd buf ~off ~len] tries to read [len] bytes from the given [fd] into
    [buf] starting at [off]. [read] {b takes} the ownership on [buf] - don't
    reuse it into another task until this one is resolved! *)

val write : file_descr -> string -> off:int -> len:int -> (int, exn) result
(** [write fd str ~off ~len] tries to write [len] bytes starting at [off] from
    [str] into the given [fd] . *)

val accept :
  ?cloexec:bool -> file_descr -> (file_descr * Unix.sockaddr, exn) result
(** [accept ?cloexec fd] is a [miou] friendly {!Unix.accept} which returns a
    new {!type:file_descr} (in a non-blocking mode). *)

val connect : file_descr -> Unix.sockaddr -> (unit, exn) result
(** [connect fd sockaddr] is a [miou] friendly {!Unix.connect}. The given [fd]
    must be in a non-blocking mode. *)

val socket :
     Unix.socket_type
  -> [ `Host of string * int | `Sockaddr of Unix.sockaddr ]
  -> (file_descr, exn) result
(** [socket c edn] allocates a new socket according to the given [edn] and the
    {!type:Unix.socket_type}. It {b set} the file descriptor in the non-blocking
    mode. *)

val of_file_descr : ?non_blocking:bool -> Unix.file_descr -> file_descr
(** [of_file_descr ?non_blocking fd] returns a {!type:file_descr} and set it to
    the non-blocking mode if [non_blocking = true] (defaults to [true]). *)
*)

(** {2 The Miou's entry point.} *)

val run : ?g:Random.State.t -> (unit -> 'a) -> 'a
