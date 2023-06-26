(** A randomized circular double linked list.

    This implementation is {b not} domain-safe and should be used into a single
    domain: otherwise, you will get data-race conditions. *)

type 'a t
type 'a node

exception Empty

val make : Random.State.t -> 'a t
val push : 'a -> 'a t -> unit
val take : 'a t -> 'a
val drop : 'a t -> unit
val length : 'a t -> int
val iter : f:('a -> unit) -> 'a t -> unit
val iter_on : f:('a node -> unit) -> 'a t -> unit
val is_empty : 'a t -> bool
val remove : 'a node -> unit
val data : 'a node -> 'a
