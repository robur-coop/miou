type 'a t

exception Empty

val enqueue : 'a t -> 'a -> unit
val dequeue : 'a t -> 'a
val make : unit -> 'a t
val is_empty : 'a t -> bool
