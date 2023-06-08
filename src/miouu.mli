val sleep : float -> (unit, exn) result
(** [sleep n] sleeps [n] second(s). *)

val run : ?g:Random.State.t -> (unit -> 'a) -> 'a
