open Miou

module Cond : sig
  (** Due to our scheduler, we must re-implement few things like "Condition" to
      be able to wait and signal (or broadcast) our tasks. The suspension of a
      task must always be notified to Miou. It is possible to use real
      {!type:Condition.t} with Miou - however, some mechanisms such as
      cancellation will not work.

      This module reimplements the {!module:Condition} with Miou. The
      behaviour remains the same, except that the user can
      {!val:Miou.cancel} a task while it is waiting for a signal/broadcast.
    *)

  type t
  (** The type of condition variables. *)

  val make : ?mutex:Mutex.t -> unit -> t
  (** [make ?mutex ()] creates and return a new condition variable. A condition
      needs a {!type:Mutex.t} to have internal inter-domain synchronization
      mechanisms. This mutex can be used by several conditions {!type:t}, so it
      is possible to specify your own {!type:Mutex.t} instead of letting Miou
      create one. *)

  val wait : predicate:(unit -> bool) -> t -> bool
  (** [wait ~predicate t] suspends the current task on the condition variable
      [t]. The task can later be woken up after the condition variable [t] has
      been signaled via {!val:signal} or {!val:broadcast}. [predicate] is a
      function which is executed {b before} the wait to test if we need to wait
      or not and {b protected} by the internal {!type:Mutex.t} (see
      {!val:make}). *)

  val until : predicate:(unit -> bool) -> fn:(unit -> 'a) -> t -> 'a
  (** [until ~predicate ~fn t] waits as long as [predicate] is [true]. Then, we
      execute [fn] as soon as the process has unblocked. The execution of
      [predicate] and [fn] is protected by the mutex internal to condition [t].
    *)

  val signal : t -> unit
  (** [signal t] wakes up one of the tasks waiting on the condition variable
      [t], if there is one. if there is none, this call has no effect. *)

  val broadcast : t -> unit
  (** [broadcast t] wakes up {b all} tasks waiting on the condition variable
      [t], if there is one. If there is none, this call has no effect. *)
end

(** {1 The Unix layer of Miou.}

    This module offers a re-implementation of the I/O according to Miou's
    model. In addition to managing possible suspensions due to I/O, the module
    also provides a notion of "ownership" which checks {i at runtime} whether
    the task is able to perform I/O on the {!type:file_descr} used. It also
    checks (again {i at runtime}) that these {!type:file_descr} have been
    released.

    For more information, please read the {!module:Miou.Ownership} module. *)

type file_descr
(** Type of file-descriptors. *)

val read : file_descr -> bytes -> off:int -> len:int -> int
(** [read fd buf ~off ~len] reads [len] bytes from [fd] into [buf] starting at
    [off]. Return the number of bytes actually read. *)

val write : file_descr -> string -> off:int -> len:int -> unit
(** [write fd str ~off ~len] writes [len] bytes starting at [off] from [str] on
    [fd]. *)

val connect : file_descr -> Unix.sockaddr -> unit
(** [connect fd sockaddr] is a Miou friendly {!val:Unix.connect}. The function
    accepts only {!type:file_descr}s in non-blocking mode. *)

val accept : ?cloexec:bool -> file_descr -> file_descr * Unix.sockaddr
(** [accept ?cloexec fd] is a Miou friendly {!Unix.accept} which returns
    file descritptors in non-blocking mode. *)

val close : file_descr -> unit
(** [close fd] closes and {!val:Miou.Ownership.disown} properly the given [fd]. Its
    use ensures that there is no leakage of resources. *)

val sleep : float -> unit
(** [sleep v] suspends the current task and {i sleeps} [v] seconds. *)

val of_file_descr :
  ?non_blocking:bool -> ?owner:Ownership.t -> Unix.file_descr -> file_descr
(** [of_file_descr ?non_blocking ?owner fd] creates a new {!type:file_descr}.
    Depending on [non_blocking] (defaults to [true]), we set the given [fd] to
    non-blocking mode or not. The user can also specify the owner of the given
    [fd]. Otherwise, we consider the current task as the owner. *)

val to_file_descr : file_descr -> Unix.file_descr
(** [to_file_descr fd] returns the {i real} {!type:Unix.file_descr}. *)

val owner : file_descr -> Ownership.t
(** [owner fd] returns the witness of the task's ownership. It is useful to pass
    the ownership to a sub-task:

    {[
      let fd = tcpv4 () in
      let p0 = Miou.call_cc ~give:[ owner fd ] @@ fun () ->
        connect fd addr; transfer fd in
      Miou.await p0
    ]} *)

val tcpv4 : unit -> file_descr
(** [tcpv4 ()] allocates a new socket owned by the current task. *)

val tcpv6 : unit -> file_descr
(** [tcpv6 ()] allocates a new socket owned by the current task. *)

val transfer : file_descr -> file_descr
(** [transfer fd] transfers the ownership of [fd] into the parent of the current
    task. *)

val disown : file_descr -> unit
(** [disown fd] informs Miou that the current task is not the owner of the
    given [fd] anymore. It's useful when you want to {i pass} the given
    [fd] to another task. *)

val bind_and_listen : ?backlog:int -> file_descr -> Unix.sockaddr -> unit
(** [bind_and_listen fd sockaddr] binds the given socket to the given
    [sockaddr] and set up the given [fd] for receiving connection requests.
    [backlog] is the maximal number of pending requests. *)

val run : ?g:Random.State.t -> ?domains:int -> (unit -> 'a) -> 'a
