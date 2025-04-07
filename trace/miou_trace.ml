let src = Logs.Src.create "miou.trace"

module Log = (val Logs.src_log src : Logs.LOG)
module Fuchsia = Fuchsia

external reraise : exn -> 'a = "%reraise"

type event =
  | Duration_begin of {
        thread: Fuchsia.thread
      ; name: string
      ; category: string
      ; ts: int64
    }
  | Duration_end of {
        thread: Fuchsia.thread
      ; name: string
      ; category: string
      ; ts: int64
    }
  | Instant of {
        thread: Fuchsia.thread
      ; name: string
      ; category: string
      ; ts: int64
    }
  | Event of { ring_id: int; event: Miou_runtime_events.event; ts: int64 }
  | Stop

let events pid =
  let queue = Miou.Queue.create () in
  let ring_thread tid = { Fuchsia.pid; tid= (tid lsl 2) + 1 } in
  let runtime_begin ring_id ts phase =
    let thread = ring_thread ring_id in
    let name = Runtime_events.runtime_phase_name phase in
    let category = "gc" in
    let ts = Runtime_events.Timestamp.to_int64 ts in
    Miou.Queue.enqueue queue (Duration_begin { thread; name; category; ts })
  in
  let runtime_end ring_id ts phase =
    let thread = ring_thread ring_id in
    let name = Runtime_events.runtime_phase_name phase in
    let category = "gc" in
    let ts = Runtime_events.Timestamp.to_int64 ts in
    Miou.Queue.enqueue queue (Duration_end { thread; name; category; ts })
  in
  let lifecycle ring_id ts event _ =
    let ts = Runtime_events.Timestamp.to_int64 ts in
    let thread = ring_thread ring_id in
    let name = Runtime_events.lifecycle_name event in
    let category = "ocaml" in
    Miou.Queue.enqueue queue (Instant { thread; name; category; ts })
  in
  let cbs =
    Runtime_events.Callbacks.create ~runtime_begin ~runtime_end ~lifecycle
      ~lost_events:(fun domain count ->
        Logs.warn (fun m -> m "[%d] losts %d events" domain count))
      ()
  in
  let fn ring_id ts event =
    let ts = Runtime_events.Timestamp.to_int64 ts in
    Miou.Queue.enqueue queue (Event { ring_id; ts; event })
  in
  (Miou_runtime_events.add_callbacks ~fn cbs, queue)

let max_sleep = 0.01
let min_sleep = 1e-6

let program_stopped ~pid =
  let pid, _status = Unix.waitpid [ WNOHANG ] pid in
  pid != 0

let rec runtime_events ~sleep ~pid queue cursor cbs =
  match Runtime_events.read_poll cursor cbs None with
  | 0 ->
      Miou_unix.sleep sleep;
      let sleep = Float.min max_sleep (sleep *. 1.2) in
      if program_stopped ~pid then Miou.Queue.enqueue queue Stop
      else runtime_events ~sleep ~pid queue cursor cbs
  | _ ->
      let sleep = Float.max min_sleep (sleep /. 1.2) in
      Miou_unix.sleep sleep;
      runtime_events ~sleep ~pid queue cursor cbs

let rec out queue ft =
  match Miou.Queue.dequeue queue with
  | Stop -> Log.debug (fun m -> m "record finished")
  | Instant { thread; name; category; ts } ->
      Fuchsia.instant_event ft ~name ~thread ~category ~ts;
      out queue ft
  | Duration_begin { thread; name; category; ts } ->
      Fuchsia.duration_begin ft ~name ~thread ~category ~ts;
      out queue ft
  | Duration_end { thread; name; category; ts } ->
      Fuchsia.duration_end ft ~name ~thread ~category ~ts;
      out queue ft
  | Event _ -> out queue ft
  | exception Miou.Queue.Empty -> Miou_unix.sleep 0.1; out queue ft

let out ?filename queue =
  let filename =
    match filename with
    | None -> Filename.temp_file "trace-" ".fxt"
    | Some filename -> filename
  in
  Log.debug (fun m -> m "trace saved into %s" filename);
  let oc = open_out filename in
  let finally () = close_out oc in
  Fun.protect ~finally @@ fun () ->
  let open Effect.Deep in
  let rec retc x = x
  and exnc exn = reraise exn
  and effc : type c. c Effect.t -> ((c, 'a) continuation -> 'b) option =
    function
    | Fuchsia.Write str ->
        output_string oc str;
        flush oc;
        Some (fun k -> continue k ())
    | _ -> None
  and handler = { retc; exnc; effc } in
  output_string oc Fuchsia.magic_number;
  match_with (out queue) (Fuchsia.create ()) handler

let rec get_cursor handle =
  Miou_unix.sleep 0.1;
  try Runtime_events.create_cursor (Some handle)
  with Failure msg ->
    Log.debug (fun m -> m "%s (will retry)" msg);
    get_cursor handle

let run ~tmp ?filename pid =
  let cbs, queue = events pid in
  Log.debug (fun m -> m "callbacks and queue initialized");
  let ic =
    Miou.call @@ fun () ->
    let cursor = get_cursor (tmp, pid) in
    Log.debug (fun m -> m "runtime events cursor initialized");
    runtime_events ~sleep:max_sleep ~pid queue cursor cbs
  in
  let oc = Miou.call @@ fun () -> out ?filename queue in
  Miou.await_all [ ic; oc ]
  |> List.iter (function
       | Ok () -> ()
       | Error exn ->
           Log.err (fun m ->
               m "Unexpected exception: %s" (Printexc.to_string exn));
           raise exn)
