type 'a t

exception Empty

val enqueue : 'a t -> 'a -> unit
val dequeue : 'a t -> 'a
val make : unit -> 'a t
val is_empty : 'a t -> bool
val iter : f:('a -> unit) -> 'a t -> unit
val drop : f:('a -> unit) -> 'a t -> unit
val length : 'a t -> int
val to_list : 'a t -> 'a list
val transfer : 'a t -> 'a t
