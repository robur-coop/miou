type thread = { pid : int; tid : int }
type t

type arg
type args = (string * arg) list

val unit : arg
val koid : int64 -> arg
val ptr : int64 -> arg
val i64 : int64 -> arg
val str : string -> arg

(**/*)

val event : t -> ?args:args -> name:string -> thread:thread -> category:string -> ts:int64 -> int -> unit
val kernel_object : t -> ?args:args  -> name:string -> int64 -> unit
