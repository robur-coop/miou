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
  | Location
  | Cancel
  | Cancelled
  | Await
  | Resume
  | Yield
  | Suspend
  | Continue
  | Attach
  | Detach
  | Exn_still_has_children
  | Exn_not_a_child
  | Exn_resource_leaked
  | Exn_not_owner
  | Run_begin
  | Run_end
  | Run_done

open Runtime_events

let spawn = User.register "miou.spawn" Spawn spawn_type
let location = User.register "miou.location" Location uid_string_uid_type
let cancel = User.register "miou.cancel" Cancel uid_type
let cancelled = User.register "miou.cancelled" Cancelled uid_type
let await = User.register "miou.await" Await uid_type
let resume = User.register "miou.resume" Resume uid_type
let yield = User.register "miou.yield" Yield uid_type
let suspend = User.register "miou.suspend" Suspend uid_string_type
let continue = User.register "miou.continue" Continue uid_string_type
let attach = User.register "miou.attach" Attach uid_uid_type
let detach = User.register "miou.detach" Detach uid_uid_type

let still_has_children =
  let name = "miou.exn.still_has_children" in
  User.register name Exn_still_has_children uid_type

let not_a_child =
  let name = "miou.exn.not_a_child" in
  User.register name Exn_not_a_child uid_uid_type

let resource_leaked =
  let name = "miou.exn.resouce_leaked" in
  User.register name Exn_resource_leaked uid_type

let not_owner =
  let name = "miou.exn.not_owner" in
  User.register name Exn_not_owner uid_uid_type

let run_begin = User.register "miou.begin" Run_begin uid_type
let run_end = User.register "miou.end" Run_end uid_type
let run_done = User.register "miou.done" Run_done uid_type

let reporter (event : Miou.Trace.event) =
  match event with
  | Miou.Trace.Spawn { uid; parent; runner; kind } ->
      Runtime_events.User.write spawn { uid; parent; runner; kind }
  | Miou.Trace.Spawn_location { uid; filename; line } ->
      Runtime_events.User.write location (uid, filename, line)
  | Miou.Trace.Cancel uid -> Runtime_events.User.write cancel uid
  | Miou.Trace.Cancelled uid -> Runtime_events.User.write cancelled uid
  | Miou.Trace.Await uid -> Runtime_events.User.write await uid
  | Miou.Trace.Resume uid -> Runtime_events.User.write resume uid
  | Miou.Trace.Yield uid -> Runtime_events.User.write yield uid
  | Miou.Trace.Suspend { name; uid } ->
      Runtime_events.User.write suspend (uid, name)
  | Miou.Trace.Continue { name; uid } ->
      Runtime_events.User.write continue (uid, name)
  | Miou.Trace.Attach { ruid; puid } ->
      Runtime_events.User.write attach (ruid, puid)
  | Miou.Trace.Detach { ruid; puid } ->
      Runtime_events.User.write detach (ruid, puid)
  | Miou.Trace.Run_begin uid -> Runtime_events.User.write run_begin uid
  | Miou.Trace.Run_end uid -> Runtime_events.User.write run_end uid
  | Miou.Trace.Run_done uid -> Runtime_events.User.write run_done uid
  | Miou.Trace.Still_has_children uid ->
      Runtime_events.User.write still_has_children uid
  | Miou.Trace.Not_a_child { self; prm } ->
      Runtime_events.User.write not_a_child (self, prm)
  | Miou.Trace.Resource_leaked uid ->
      Runtime_events.User.write resource_leaked uid
  | Miou.Trace.Not_owner { ruid; puid } ->
      Runtime_events.User.write not_owner (ruid, puid)
  | _ -> ()

let add_callbacks ~fn callbacks =
  let create_spawn ring_id ts ev spawn =
    match Runtime_events.User.tag ev with
    | Spawn ->
        let { uid; runner; parent; kind } = spawn in
        let event = Miou.Trace.Spawn { uid; runner; parent; kind } in
        fn ring_id ts event
    | _ -> ()
  in
  let from_uid ring_id ts ev uid =
    match Runtime_events.User.tag ev with
    | Cancel -> fn ring_id ts (Miou.Trace.Cancel uid)
    | Cancelled -> fn ring_id ts (Miou.Trace.Cancelled uid)
    | Await -> fn ring_id ts (Miou.Trace.Await uid)
    | Resume -> fn ring_id ts (Miou.Trace.Resume uid)
    | Yield -> fn ring_id ts (Miou.Trace.Yield uid)
    | Run_begin -> fn ring_id ts (Miou.Trace.Run_begin uid)
    | Run_end -> fn ring_id ts (Miou.Trace.Run_end uid)
    | Run_done -> fn ring_id ts (Miou.Trace.Run_done uid)
    | Exn_still_has_children ->
        fn ring_id ts (Miou.Trace.Still_has_children uid)
    | Exn_resource_leaked -> fn ring_id ts (Miou.Trace.Resource_leaked uid)
    | _ -> ()
  in
  let from_uid_string ring_id ts ev (uid, name) =
    match Runtime_events.User.tag ev with
    | Suspend -> fn ring_id ts (Miou.Trace.Suspend { name; uid })
    | Continue -> fn ring_id ts (Miou.Trace.Continue { name; uid })
    | _ -> ()
  in
  let from_uid_uid ring_id ts ev (uid0, uid1) =
    match Runtime_events.User.tag ev with
    | Attach -> fn ring_id ts (Miou.Trace.Attach { ruid= uid0; puid= uid1 })
    | Detach -> fn ring_id ts (Miou.Trace.Detach { ruid= uid0; puid= uid1 })
    | Exn_not_a_child ->
        fn ring_id ts (Miou.Trace.Not_a_child { self= uid0; prm= uid1 })
    | Exn_not_owner ->
        fn ring_id ts (Miou.Trace.Not_owner { ruid= uid0; puid= uid1 })
    | _ -> ()
  in
  let from_uid_string_uid ring_id ts ev (uid0, str, uid1) =
    match Runtime_events.User.tag ev with
    | Location ->
        let event =
          Miou.Trace.Spawn_location { uid= uid0; filename= str; line= uid1 }
        in
        fn ring_id ts event
    | _ -> ()
  in
  callbacks
  |> Runtime_events.Callbacks.add_user_event spawn_type create_spawn
  |> Runtime_events.Callbacks.add_user_event uid_type from_uid
  |> Runtime_events.Callbacks.add_user_event uid_string_type from_uid_string
  |> Runtime_events.Callbacks.add_user_event uid_uid_type from_uid_uid
  |> Runtime_events.Callbacks.add_user_event uid_string_uid_type
       from_uid_string_uid
