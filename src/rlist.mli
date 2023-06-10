(** A randomized circular double linked list.

    This implementation is {b not} thread-safe and should be used into a single
    domain: otherwise, you will get data-race conditions. *)

type 'a t

exception Empty

val make : Random.State.t -> 'a t
val push : 'a -> 'a t -> unit
val take : 'a t -> 'a
val length : 'a t -> int
val iter : f:('a -> unit) -> 'a t -> unit
val is_empty : 'a t -> bool
