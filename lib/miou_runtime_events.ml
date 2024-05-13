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
      | Some { filename; line_number; start_char; end_char; _ } ->
          ( Some filename
          , Simple { line= line_number; start= start_char; stop= end_char } )
    in
    { name; filename; location }

  let current () =
    let bt = Printexc.get_callstack max_int in
    match Printexc.backtrace_slots bt with
    | None -> [||]
    | Some [| _current |] -> [||]
    | Some bt -> Array.map to_entry Array.(sub bt 1 (length bt - 1))
end

type k = [ `Parallel | `Concurrent ]
type spawn = { k: k; resources: int; uid: int; parent: int; runner: int }

let spawn_type =
  let encode buf { k; resources; uid; parent; runner } =
    let hdr = match k with `Parallel -> 0x80 | `Concurrent -> 0x0 in
    let resources = min resources 0x7f in
    let hdr = hdr lor resources in
    Bytes.set_int8 buf 0 hdr;
    Bytes.set_int64_le buf 1 (Int64.of_int uid);
    Bytes.set_int64_le buf 9 (Int64.of_int parent);
    Bytes.set_int64_le buf 17 (Int64.of_int runner);
    25
  in
  let decode buf _size =
    let hdr = Bytes.get_int8 buf 0 in
    let k = match hdr land 0x80 with 0 -> `Concurrent | _ -> `Parallel in
    let resources = hdr land 0x7f in
    let uid = Int64.to_int (Bytes.get_int64_le buf 1) in
    let parent = Int64.to_int (Bytes.get_int64_le buf 9) in
    let runner = Int64.to_int (Bytes.get_int64_le buf 17) in
    { k; resources; uid; parent; runner }
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

type Runtime_events.User.tag += Spawn | Cancel | Await | Resume | Cancelled

let spawn = Runtime_events.User.register "miou.spawn" Spawn spawn_type
let cancel = Runtime_events.User.register "miou.cancel" Cancel task_type
let await = Runtime_events.User.register "miou.await" Await task_type
let resume = Runtime_events.User.register "miou.resume" Resume task_type

let cancelled =
  Runtime_events.User.register "miou.cancelled" Cancelled task_type

type event =
  | Spawn of spawn
  | Cancel of task
  | Await of task
  | Resume of task
  | Cancelled of task

let add_callbacks ~fn x =
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
    | _ -> ()
  in
  x
  |> Runtime_events.Callbacks.add_user_event spawn_type create_spawn
  |> Runtime_events.Callbacks.add_user_event task_type from_task
