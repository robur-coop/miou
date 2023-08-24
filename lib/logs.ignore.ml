type level = App | Error | Warning | Info | Debug

type ('a, 'b) msgf =
     (   ?domain:Domain.id
      -> ?header:string
      -> ('a, Format.formatter, unit, 'b) format4
      -> 'a)
  -> 'b

let kmsg : type a b. (unit -> b) -> level -> (a, b) msgf -> b =
 fun k _level _msgf -> k ()

let msg level msgf = kmsg (Fun.const ()) level msgf
let debug msgf = msg Debug msgf
let err msgf = msg Error msgf
let warn msgf = msg Warning msgf
