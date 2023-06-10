(** A randomized circular double linked list. *)

type 'a t

exception Empty

val make : Random.State.t -> 'a t
val push : 'a -> 'a t -> unit
val take : 'a t -> 'a
val length : 'a t -> int
val iter : f:('a -> unit) -> 'a t -> unit
val is_empty : 'a t -> bool
