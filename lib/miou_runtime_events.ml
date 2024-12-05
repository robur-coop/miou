(*
module Location = struct
  type t = Nowhere | Simple of { line: int; start: int; stop: int }

  let encode buf ~off = function
    | Nowhere -> Bytes.set_int8 buf off 0; 1
    | Simple { line; start; stop } ->
        Bytes.set_int8 buf off 1;
        Bytes.set_int64_le buf (off + 1) (Int64.of_int line);
        Bytes.set_int64_le buf (off + 9) (Int64.of_int start);
        Bytes.set_int64_le buf (off + 17) (Int64.of_int stop);
        25

  let decode buf ~off _size =
    match Bytes.get_int8 buf 0 with
    | 0 -> Nowhere
    | _ ->
        let line = Int64.to_int (Bytes.get_int64_le buf (off + 1)) in
        let start = Int64.to_int (Bytes.get_int64_le buf (off + 9)) in
        let stop = Int64.to_int (Bytes.get_int64_le buf (off + 17)) in
        Simple { line; start; stop }

  let size = function Nowhere -> 1 | Simple _ -> 25
end

module Callstack = struct
  type entry = {
      name: string option
    ; filename: string option
    ; location: Location.t
  }

  type t = entry array

  let encode_optional_string buf ~off = function
    | None | Some "" ->
        Bytes.set_int64_le buf off Int64.zero;
        8
    | Some str ->
        let len = Int64.of_int (String.length str) in
        Bytes.set_int64_le buf off len;
        Bytes.blit_string str 0 buf (off + 8) (Int64.to_int len);
        8 + Int64.to_int len

  let decode_optional_string buf ~off _size =
    match Bytes.get_int64_le buf off with
    | 0L -> None
    | len ->
        let len = Int64.to_int len in
        let res = Bytes.make len '\000' in
        Bytes.blit buf (off + 8) res 0 len;
        Some (Bytes.unsafe_to_string res)

  let size_of_optional_string = function
    | None | Some "" -> 8
    | Some str -> 8 + String.length str

  let encode_entry buf ~off { name; filename; location } =
    let m0 = encode_optional_string buf ~off name in
    let m1 = encode_optional_string buf ~off:(off + m0) filename in
    let m2 = Location.encode buf ~off:(off + m0 + m1) location in
    m0 + m1 + m2

  let decode_entry buf ~off _size =
    let name = decode_optional_string buf ~off _size in
    let filename =
      decode_optional_string buf ~off:(off + size_of_optional_string name) _size
    in
    let location =
      let off =
        off + size_of_optional_string name + size_of_optional_string filename
      in
      Location.decode buf ~off _size
    in
    { name; filename; location }

  let size_of_entry { name; filename; location } =
    size_of_optional_string name
    + size_of_optional_string filename
    + Location.size location

  let encode buf ~off = function
    | [||] ->
        Bytes.set_int64_le buf off Int64.zero;
        8
    | arr ->
        let len = Int64.of_int (Array.length arr) in
        Bytes.set_int64_le buf off len;
        let m = ref 0 in
        for i = 0 to Array.length arr - 1 do
          m := !m + encode_entry buf ~off:(off + 8 + !m) arr.(i)
        done;
        8 + !m

  let decode buf ~off _size =
    match Bytes.get_int64_le buf off with
    | 0L -> [||]
    | len ->
        let cursor = ref 8 in
        let init _idx =
          let entry = decode_entry buf ~off:(off + !cursor) _size in
          let len = size_of_entry entry in
          cursor := !cursor + len;
          entry
        in
        Array.init (Int64.to_int len) init

  let to_entry slot =
    let name = Printexc.Slot.name slot in
    let filename, location =
      match Printexc.Slot.location slot with
      | None -> (None, Location.Nowhere)
      | Some location ->
          let filename = location.filename
          and line = location.line_number
          and start = location.start_char
          and stop = location.end_char in
          ( Some filename
          , Simple { line; start; stop } )
    in
    { name; filename; location }

  let current () =
    let bt = Printexc.get_callstack max_int in
    match Printexc.backtrace_slots bt with
    | None -> [||]
    | Some [| _current |] -> [||]
    | Some bt -> Array.map to_entry Array.(sub bt 1 (length bt - 1))
end
*)

type spawn = { uid: int; parent: int; runner: int }

let spawn_type =
  let encode buf { uid; parent; runner } =
    Bytes.set_int64_le buf 0 (Int64.of_int uid);
    Bytes.set_int64_le buf 8 (Int64.of_int parent);
    Bytes.set_int64_le buf 16 (Int64.of_int runner);
    24
  in
  let decode buf _size =
    let uid = Int64.to_int (Bytes.get_int64_le buf 0) in
    let parent = Int64.to_int (Bytes.get_int64_le buf 8) in
    let runner = Int64.to_int (Bytes.get_int64_le buf 16) in
    { uid; parent; runner }
  in
  Runtime_events.Type.register ~encode ~decode

type task = { uid: int; runner: int }

let task_type =
  let encode buf { uid; runner } =
    Bytes.set_int64_le buf 0 (Int64.of_int uid);
    Bytes.set_int64_le buf 8 (Int64.of_int runner);
    16
  in
  let decode buf _size =
    let uid = Bytes.get_int64_le buf 0 in
    let runner = Bytes.get_int64_le buf 8 in
    { uid= Int64.to_int uid; runner= Int64.to_int runner }
  in
  Runtime_events.Type.register ~encode ~decode

type resource = int

let resource_and_task_type =
  let encode buf (uid, task) =
    Bytes.set_int64_le buf 0 (Int64.of_int uid);
    Bytes.set_int64_le buf 8 (Int64.of_int task.uid);
    Bytes.set_int64_le buf 16 (Int64.of_int task.runner);
    24
  in
  let decode buf _size =
    let uid = Bytes.get_int64_le buf 0 in
    let task_uid = Bytes.get_int64_le buf 8 in
    let task_runner = Bytes.get_int64_le buf 16 in
    let task =
      { uid= Int64.to_int task_uid; runner= Int64.to_int task_runner }
    in
    (Int64.to_int uid, task)
  in
  Runtime_events.Type.register ~encode ~decode

type syscall = int

let syscall_and_task_type =
  let encode buf (uid, task) =
    Bytes.set_int64_le buf 0 (Int64.of_int uid);
    Bytes.set_int64_le buf 8 (Int64.of_int task.uid);
    Bytes.set_int64_le buf 16 (Int64.of_int task.runner);
    24
  in
  let decode buf _size =
    let uid = Bytes.get_int64_le buf 0 in
    let task_uid = Bytes.get_int64_le buf 8 in
    let task_runner = Bytes.get_int64_le buf 16 in
    let task =
      { uid= Int64.to_int task_uid; runner= Int64.to_int task_runner }
    in
    (Int64.to_int uid, task)
  in
  Runtime_events.Type.register ~encode ~decode

type error =
  | Still_has_children
  | Not_a_child of { child: task }
  | Not_owner of resource
  | Resource_leaked of resource

let task_and_error_type =
  let encode buf (task, error) =
    Bytes.set_int64_le buf 0 (Int64.of_int task.uid);
    Bytes.set_int64_le buf 8 (Int64.of_int task.runner);
    match error with
    | Still_has_children -> Bytes.set_uint8 buf 16 0; 17
    | Not_a_child { child= { uid; runner } } ->
        Bytes.set_uint8 buf 16 1;
        Bytes.set_int64_le buf 17 (Int64.of_int uid);
        Bytes.set_int64_le buf 25 (Int64.of_int runner);
        33
    | Not_owner uid ->
        Bytes.set_uint8 buf 16 2;
        Bytes.set_int64_le buf 17 (Int64.of_int uid);
        25
    | Resource_leaked uid ->
        Bytes.set_uint8 buf 16 3;
        Bytes.set_int64_le buf 17 (Int64.of_int uid);
        25
  in
  let decode buf _size =
    let task_uid = Bytes.get_int64_le buf 0 in
    let task_runner = Bytes.get_int64_le buf 8 in
    let task =
      { uid= Int64.to_int task_uid; runner= Int64.to_int task_runner }
    in
    match Bytes.get_uint8 buf 16 with
    | 0 -> (task, Still_has_children)
    | 1 ->
        let child_uid = Bytes.get_int64_le buf 17 in
        let child_runner = Bytes.get_int64_le buf 25 in
        let child =
          { uid= Int64.to_int child_uid; runner= Int64.to_int child_runner }
        in
        (task, Not_a_child { child })
    | 2 ->
        let uid = Bytes.get_int64_le buf 17 in
        (task, Not_owner (Int64.to_int uid))
    | 3 ->
        let uid = Bytes.get_int64_le buf 17 in
        (task, Resource_leaked (Int64.to_int uid))
    | _ -> invalid_arg "Miou_runtime_event: invalid type of error"
  in
  Runtime_events.Type.register ~encode ~decode

type Runtime_events.User.tag +=
  | Spawn
  | Cancel
  | Await
  | Resume
  | Cancelled
  | Yield
  | Suspend
  | Unblock
  | Attach
  | Detach
  | Abort

open Runtime_events

let spawn = User.register "miou.spawn" Spawn spawn_type
let cancel = User.register "miou.cancel" Cancel task_type
let await = User.register "miou.await" Await task_type
let resume = User.register "miou.resume" Resume task_type
let cancelled = User.register "miou.cancelled" Cancelled task_type
let yield = User.register "miou.yield" Yield task_type
let suspend = User.register "miou.suspend" Suspend syscall_and_task_type
let unblock = User.register "miou.unblock" Unblock syscall_and_task_type
let attach = User.register "miou.attach" Attach resource_and_task_type
let detach = User.register "miou.detach" Detach resource_and_task_type
let abort = User.register "miou.abort" Abort task_and_error_type

type event =
  | Spawn of spawn
  | Cancel of task
  | Await of task
  | Resume of task
  | Cancelled of task
  | Yield of task
  | Suspend of syscall * task
  | Unblock of syscall * task
  | Attach of resource * task
  | Detach of resource * task
  | Abort of task * error

let add_callbacks ~fn callbacks =
  let create_spawn ring_id ts ev spawn =
    match Runtime_events.User.tag ev with
    | Spawn -> fn ring_id ts (Spawn spawn)
    | _ -> ()
  in
  let from_task ring_id ts ev task =
    match Runtime_events.User.tag ev with
    | Cancel -> fn ring_id ts (Cancel task)
    | Await -> fn ring_id ts (Await task)
    | Resume -> fn ring_id ts (Resume task)
    | Cancelled -> fn ring_id ts (Cancelled task)
    | Yield -> fn ring_id ts (Yield task)
    | _ -> ()
  in
  let from_syscall_and_task ring_id ts ev (syscall, task) =
    match Runtime_events.User.tag ev with
    | Suspend -> fn ring_id ts (Suspend (syscall, task))
    | Unblock -> fn ring_id ts (Unblock (syscall, task))
    | _ -> ()
  in
  let from_resource_and_task ring_id ts ev (resource, task) =
    match Runtime_events.User.tag ev with
    | Attach -> fn ring_id ts (Attach (resource, task))
    | Detach -> fn ring_id ts (Detach (resource, task))
    | _ -> ()
  in
  let from_task_and_error ring_id ts ev (task, error) =
    match Runtime_events.User.tag ev with
    | Abort -> fn ring_id ts (Abort (task, error))
    | _ -> ()
  in
  callbacks
  |> Runtime_events.Callbacks.add_user_event spawn_type create_spawn
  |> Runtime_events.Callbacks.add_user_event task_type from_task
  |> Runtime_events.Callbacks.add_user_event syscall_and_task_type
       from_syscall_and_task
  |> Runtime_events.Callbacks.add_user_event resource_and_task_type
       from_resource_and_task
  |> Runtime_events.Callbacks.add_user_event task_and_error_type
       from_task_and_error
