type spawn = { uid: int; parent: int; runner: int }
type task = { uid: int; runner: int }
type resource = int
type syscall = int

type error =
  | Still_has_children
  | Not_a_child of { child: task }
  | Not_owner of resource
  | Resource_leaked of resource

type event =
  | Spawn of spawn (* call / async *)
  | Cancel of task (* cancel *)
  | Await of task (* await / await_exn / await_all / await_one / await_first *)
  | Resume of task
  | Cancelled of task
  | Yield of task (* yield *)
  | Suspend of syscall * task (* suspend *)
  | Unblock of syscall * task
  | Attach of resource * task (* own / transfer *)
  | Detach of resource * task (* disown / transfer *)
  | Abort of task * error

val spawn : spawn Runtime_events.User.t
val cancel : task Runtime_events.User.t
val await : task Runtime_events.User.t
val resume : task Runtime_events.User.t
val cancelled : task Runtime_events.User.t
val yield : task Runtime_events.User.t
val suspend : (syscall * task) Runtime_events.User.t
val unblock : (syscall * task) Runtime_events.User.t
val attach : (resource * task) Runtime_events.User.t
val detach : (resource * task) Runtime_events.User.t
val abort : (task * error) Runtime_events.User.t

val add_callbacks :
     fn:(int -> Runtime_events.Timestamp.t -> event -> unit)
  -> Runtime_events.Callbacks.t
  -> Runtime_events.Callbacks.t
