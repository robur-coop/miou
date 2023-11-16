val failwith : ('a, Format.formatter, unit, 'b) format4 -> 'a
val invalid_arg : ('a, Format.formatter, unit, 'b) format4 -> 'a

type 'a t = Format.formatter -> 'a -> unit

val pf : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
val str : ('a, Format.formatter, unit, string) format4 -> 'a
val kstr : (string -> 'a) -> ('b, Format.formatter, unit, 'a) format4 -> 'b
val concat : ?sep:unit t -> 'a t list -> 'a t

(**/**)

val string : string t
val int : int t
val space : 'a t
val semi : 'a t
val iter : ?sep:unit t -> (('a -> unit) -> 'b -> unit) -> 'a t -> 'b t

module Dump : sig
  val iter : (('a -> unit) -> 'b -> unit) -> 'b t -> 'a t -> 'b t
  val list : 'a t -> 'a list t
end
