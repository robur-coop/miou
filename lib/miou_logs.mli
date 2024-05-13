type level = App | Error | Warning | Info | Debug

type ('a, 'b) msgf =
     (   ?domain:Domain.id
      -> ?header:string
      -> ('a, Format.formatter, unit, 'b) format4
      -> 'a)
  -> 'b

val msg : level -> ('a, unit) msgf -> unit
val debug : ('a, unit) msgf -> unit
val err : ('a, unit) msgf -> unit
val warn : ('a, unit) msgf -> unit

module Make (_ : sig
  val src : string
end) : sig
  val msg : level -> ('a, unit) msgf -> unit
  val debug : ('a, unit) msgf -> unit
  val err : ('a, unit) msgf -> unit
  val warn : ('a, unit) msgf -> unit
end
