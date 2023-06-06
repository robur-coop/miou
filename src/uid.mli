type t

val of_int : int -> t
val to_int : t -> int
val pp : Format.formatter -> t -> unit

(** *)

val parallel : unit -> t
val concurrent : unit -> t
val reset : unit -> unit

(** *)

type _ Effect.t += Uid : t Effect.t
