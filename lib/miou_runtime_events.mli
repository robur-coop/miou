type spawn = {
    uid: int
  ; parent: int
  ; runner: int
  ; kind: [ `Async | `Parallel ]
}

type event =
  | Spawn of spawn
  | Spawn_location of int * string * int
  | Cancel of int
  | Await of int
  | Resume of int
  | Cancelled of int
  | Yield of int
  | Suspend of int * string
  | Unblock of int * string
  | Attach of int * int
  | Detach of int * int
  | Still_has_children of int
  | Not_a_child of int * int
  | Begin of int
  | End of int
  | Finish of int

val spawn : spawn Runtime_events.User.t
val spawn_location : (int * string * int) Runtime_events.User.t
val cancel : int Runtime_events.User.t
val await : int Runtime_events.User.t
val resume : int Runtime_events.User.t
val cancelled : int Runtime_events.User.t
val yield : int Runtime_events.User.t
val suspend : (int * string) Runtime_events.User.t
val unblock : (int * string) Runtime_events.User.t
val attach : (int * int) Runtime_events.User.t
val detach : (int * int) Runtime_events.User.t
val still_has_children : int Runtime_events.User.t
val not_a_child : (int * int) Runtime_events.User.t
val prm_begin : int Runtime_events.User.t
val prm_end : int Runtime_events.User.t
val prm_finish : int Runtime_events.User.t

val add_callbacks :
     fn:(int -> Runtime_events.Timestamp.t -> event -> unit)
  -> Runtime_events.Callbacks.t
  -> Runtime_events.Callbacks.t
