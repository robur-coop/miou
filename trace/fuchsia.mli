type _ Effect.t += Write : string -> unit Effect.t
type thread = { pid: int; tid: int }
type t
type arg
type args = (string * arg) list

val magic_number : string
val unit : arg
val koid : int64 -> arg
val ptr : int64 -> arg
val i64 : int64 -> arg
val str : string -> arg

(**/*)

val event :
     t
  -> ?args:args
  -> name:string
  -> thread:thread
  -> category:string
  -> ts:int64
  -> int
  -> unit

val instant_event :
  t -> name:string -> thread:thread -> category:string -> ts:int64 -> unit

val duration_begin :
  t -> name:string -> thread:thread -> category:string -> ts:int64 -> unit

val duration_end :
  t -> name:string -> thread:thread -> category:string -> ts:int64 -> unit

val kernel_object : t -> ?args:args -> name:string -> int64 -> unit
val create : unit -> t
