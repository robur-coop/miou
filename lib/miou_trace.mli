type event = ..

type event +=
  | Spawn of { uid : int; parent : int; runner : int; kind : [ `Async | `Parallel ] }
  | Spawn_location of { uid : int; filename : string; line : int }
  | Cancel of int
  | Cancelled of int
  | Await of int
  | Resume of int
  | Yield of int
  | Suspend of { name : string; uid : int }
  | Continue of { name : string; uid : int }
  | Attach of { ruid : int; puid : int }
  | Detach of { ruid : int; puid : int }
  | Run_begin of int
  | Run_end of int
  | Run_done of int
  | Still_has_children of int
  | Not_a_child of { self : int; prm : int }
  | Resource_leaked of int
  | Not_owner of { ruid : int; puid : int }

val set_reporter : (event -> unit) -> unit
val trace : event -> unit
