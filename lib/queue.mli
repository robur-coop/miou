type 'a t
(** The type of queues containing elements of type ['a]. *)

exception Empty
(** Raised when {!dequeue} is applied to an empty queue. *)

val create : unit -> 'a t
(** Return a new queue, initially empty. *)

val enqueue : 'a t -> 'a -> unit
(** [enqueue q x] adds the element [x] at the end of the queue [q]. *)

val dequeue : 'a t -> 'a
(** [dequeue q] removes and returns the first element in queue [q], or raises
    {!exception:Empty} if the queue is empty. *)

val is_empty : 'a t -> bool
(** Return [true] if the given queue is empty, [false] otherwise. *)

val iter : f:('a -> unit) -> 'a t -> unit
(** [iter ~f q] applies [f] in turn to all elements of [q], from the least
    recently entered to the most recently entered. The queue itself is
    unchanged. *)

val drop : f:('a -> unit) -> 'a t -> unit
(** [drop ~f q] applies [f] in turn to all elements of [q] {b and} discard all
    elements of [q]. *)

val length : 'a t -> int
(** Return the number of elements in a queue. *)

val to_list : 'a t -> 'a list
(** [to_list q] returns a list of [q]'s elements. *)

val transfer : 'a t -> 'a t
(** [transfer q] returns new queue in which we have added all of [q]'s elements,
    then clears [q]. It's an atomically safe equivalent to the sequence
    [let q' = make () in drop ~f:(fun x -> enqueue q x) q'; q']. *)
