type kind = [ `Parallel | `Concurrent ]
type spawn = { kind: kind; resources: int; uid: int; parent: int; runner: int }
type task = { uid: int; runner: int }

type event =
  | Spawn of spawn
  | Cancel of task
  | Await of task
  | Resume of task
  | Cancelled of task

val spawn : spawn Runtime_events.User.t
val cancel : task Runtime_events.User.t
val await : task Runtime_events.User.t
val resume : task Runtime_events.User.t
val cancelled : task Runtime_events.User.t

val add_callbacks :
     fn:(int -> Runtime_events.Timestamp.t -> event -> unit)
  -> Runtime_events.Callbacks.t
  -> Runtime_events.Callbacks.t
