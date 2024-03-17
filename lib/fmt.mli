(* Copyright (c) 2016 The fmt programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*)

val failwith : ('a, Format.formatter, unit, 'b) format4 -> 'a
val invalid_arg : ('a, Format.formatter, unit, 'b) format4 -> 'a

type 'a t = Format.formatter -> 'a -> unit

val pf : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
val str : ('a, Format.formatter, unit, string) format4 -> 'a
val kstr : (string -> 'a) -> ('b, Format.formatter, unit, 'a) format4 -> 'b
val concat : ?sep:unit t -> 'a t list -> 'a t
val fmt : ('a, Format.formatter, unit) Stdlib.format -> Format.formatter -> 'a

(**/**)

val string : string t
val int : int t
val space : 'a t
val semi : 'a t
val iter : ?sep:unit t -> (('a -> unit) -> 'b -> unit) -> 'a t -> 'b t

module Dump : sig
  val iter : (('a -> unit) -> 'b -> unit) -> 'b t -> 'a t -> 'b t
  val list : 'a t -> 'a list t
  val hashtbl : 'a t -> 'b t -> ('a, 'b) Hashtbl.t t
  val option : 'a t -> 'a option t
end
