module Make () : sig
  type t = private int [@@immediate]

  val null : t
  val pp : t Miou_fmt.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val gen : unit -> t
  val reset : unit -> unit
  external of_int : int -> t = "%identity"
  external to_int : t -> int = "%identity"
end
