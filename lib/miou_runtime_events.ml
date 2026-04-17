type spawn = {
    uid: int
  ; parent: int
  ; runner: int
  ; kind: [ `Async | `Parallel ]
}

let spawn_type =
  let encode buf { uid; parent; runner; kind } =
    Bytes.set_int64_le buf 0 (Int64.of_int uid);
    Bytes.set_int64_le buf 8 (Int64.of_int parent);
    Bytes.set_int64_le buf 16 (Int64.of_int runner);
    let kind = match kind with `Async -> 1 | `Parallel -> 2 in
    Bytes.set_uint8 buf 24 kind;
    25
  in
  let decode buf _size =
    let uid = Int64.to_int (Bytes.get_int64_le buf 0) in
    let parent = Int64.to_int (Bytes.get_int64_le buf 8) in
    let runner = Int64.to_int (Bytes.get_int64_le buf 16) in
    let kind =
      match Bytes.get_uint8 buf 24 with
      | 1 -> `Async
      | 2 -> `Parallel
      | _ -> failwith "Invalid spawn type"
    in
    { uid; parent; runner; kind }
  in
  Runtime_events.Type.register ~encode ~decode

let uid_string_type =
  let encode buf (uid, str) =
    Bytes.set_int64_le buf 0 (Int64.of_int uid);
    let len = Int.min (Bytes.length buf - 16) (String.length str) in
    Bytes.set_int64_le buf 8 (Int64.of_int len);
    Bytes.blit_string str 0 buf 16 len;
    16 + len
  in
  let decode buf _size =
    let uid = Int64.to_int (Bytes.get_int64_le buf 0) in
    let len = Int64.to_int (Bytes.get_int64_le buf 8) in
    let str = Bytes.create len in
    Bytes.blit buf 16 str 0 len;
    (uid, Bytes.unsafe_to_string str)
  in
  Runtime_events.Type.register ~encode ~decode

let uid_string_uid_type =
  let encode buf (uid0, str, uid1) =
    Bytes.set_int64_le buf 0 (Int64.of_int uid0);
    Bytes.set_int64_le buf 8 (Int64.of_int uid1);
    let len = Int.min (Bytes.length buf - 24) (String.length str) in
    Bytes.set_int64_le buf 16 (Int64.of_int len);
    Bytes.blit_string str 0 buf 24 len;
    24 + len
  in
  let decode buf _size =
    let uid0 = Int64.to_int (Bytes.get_int64_le buf 0) in
    let uid1 = Int64.to_int (Bytes.get_int64_le buf 8) in
    let len = Int64.to_int (Bytes.get_int64_le buf 16) in
    let str = Bytes.create len in
    Bytes.blit buf 24 str 0 len;
    (uid0, Bytes.unsafe_to_string str, uid1)
  in
  Runtime_events.Type.register ~encode ~decode

let uid_type =
  let encode buf uid =
    Bytes.set_int64_le buf 0 (Int64.of_int uid);
    8
  in
  let decode buf _size = Int64.to_int (Bytes.get_int64_le buf 0) in
  Runtime_events.Type.register ~encode ~decode

let uid_uid_type =
  let encode buf (uid0, uid1) =
    Bytes.set_int64_le buf 0 (Int64.of_int uid0);
    Bytes.set_int64_le buf 8 (Int64.of_int uid1);
    16
  in
  let decode buf _size =
    let uid0 = Int64.to_int (Bytes.get_int64_le buf 0) in
    let uid1 = Int64.to_int (Bytes.get_int64_le buf 8) in
    (uid0, uid1)
  in
  Runtime_events.Type.register ~encode ~decode

type Runtime_events.User.tag +=
  | Spawn
  | Spawn_location
  | Cancel
  | Await
  | Resume
  | Cancelled
  | Yield
  | Suspend
  | Unblock
  | Attach
  | Detach
  | Exn_still_has_children
  | Exn_not_a_child
  | Begin
  | End
  | Finish

open Runtime_events

let spawn = User.register "miou.spawn" Spawn spawn_type

let spawn_location =
  User.register "miou.spawn-location" Spawn_location uid_string_uid_type

let cancel = User.register "miou.cancel" Cancel uid_type
let await = User.register "miou.await" Await uid_type
let resume = User.register "miou.resume" Resume uid_type
let cancelled = User.register "miou.cancelled" Cancelled uid_type
let yield = User.register "miou.yield" Yield uid_type
let suspend = User.register "miou.suspend" Suspend uid_string_type
let unblock = User.register "miou.unblock" Unblock uid_string_type
let attach = User.register "miou.attach" Attach uid_uid_type
let detach = User.register "miou.detach" Detach uid_uid_type

let still_has_children =
  User.register "miou.exn.still_has_children" Exn_still_has_children uid_type

let not_a_child =
  User.register "miou.exn.not_a_child" Exn_not_a_child uid_uid_type

let prm_begin = User.register "miou.begin" Begin uid_type
let prm_end = User.register "miou.end" End uid_type
let prm_finish = User.register "miou.finish" Finish uid_type

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

let add_callbacks ~fn callbacks =
  let create_spawn ring_id ts ev spawn =
    match Runtime_events.User.tag ev with
    | Spawn -> fn ring_id ts (Spawn spawn)
    | _ -> ()
  in
  let from_uid ring_id ts ev uid =
    match Runtime_events.User.tag ev with
    | Cancel -> fn ring_id ts (Cancel uid)
    | Await -> fn ring_id ts (Await uid)
    | Resume -> fn ring_id ts (Resume uid)
    | Cancelled -> fn ring_id ts (Cancelled uid)
    | Yield -> fn ring_id ts (Yield uid)
    | Begin -> fn ring_id ts (Begin uid)
    | End -> fn ring_id ts (End uid)
    | Finish -> fn ring_id ts (Finish uid)
    | Exn_still_has_children -> fn ring_id ts (Still_has_children uid)
    | _ -> ()
  in
  let from_uid_string ring_id ts ev (uid, str) =
    match Runtime_events.User.tag ev with
    | Suspend -> fn ring_id ts (Suspend (uid, str))
    | Unblock -> fn ring_id ts (Unblock (uid, str))
    | _ -> ()
  in
  let from_uid_uid ring_id ts ev (uid0, uid1) =
    match Runtime_events.User.tag ev with
    | Attach -> fn ring_id ts (Attach (uid0, uid1))
    | Detach -> fn ring_id ts (Detach (uid0, uid1))
    | Exn_not_a_child -> fn ring_id ts (Not_a_child (uid0, uid1))
    | _ -> ()
  in
  let from_uid_string_uid ring_id ts ev (uid0, str, uid1) =
    match Runtime_events.User.tag ev with
    | Spawn_location -> fn ring_id ts (Spawn_location (uid0, str, uid1))
    | _ -> ()
  in
  callbacks
  |> Runtime_events.Callbacks.add_user_event spawn_type create_spawn
  |> Runtime_events.Callbacks.add_user_event uid_type from_uid
  |> Runtime_events.Callbacks.add_user_event uid_string_type from_uid_string
  |> Runtime_events.Callbacks.add_user_event uid_uid_type from_uid_uid
  |> Runtime_events.Callbacks.add_user_event uid_string_uid_type
       from_uid_string_uid
