type event =
  | Duration_begin of {
        thread: Fushia.thread
      ; name: string
      ; category: string
      ; ts: int64
    }
  | Duration_end of {
        thread: Fushia.thread
      ; name: string
      ; category: string
      ; ts: int64
    }
  | Instant of {
        thread: Fushia.thread
      ; name: string
      ; category: string
      ; ts: int64
    }
  | Event of { ring_id: int; event: Miou_runtime_events.event; ts: int64 }
  | Stop

let events pid =
  let queue = Miou.Queue.create () in
  let ring_thread tid = { Fushia.pid; tid= (tid lsl 2) + 1 } in
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

let program_stopped ~program =
  let pid, _status = Unix.waitpid [ WNOHANG ] program in
  pid != 0

let rec runtime_events ~sleep ~program queue cursor cbs =
  match Runtime_events.read_poll cursor cbs None with
  | 0 ->
      Miou_unix.sleep sleep;
      let sleep = Float.min max_sleep (sleep *. 1.2) in
      if program_stopped ~program then Miou.Queue.enqueue queue Stop
      else runtime_events ~sleep ~program queue cursor cbs
  | _ ->
      let sleep = Float.max min_sleep (sleep /. 1.2) in
      Miou_unix.sleep sleep;
      runtime_events ~sleep ~program queue cursor cbs

let rec out queue =
  match Miou.Queue.dequeue queue with Stop -> () | _event -> out queue

let run ~program handle =
  let cbs, queue = events program in
  let ic =
    Miou.call @@ fun () ->
    let cursor = Runtime_events.create_cursor (Some handle) in
    runtime_events ~sleep:max_sleep ~program queue cursor cbs
  in
  let oc = Miou.call @@ fun () -> out queue in
  Miou.await_all [ ic; oc ]
  |> List.iter (function Ok () -> () | Error exn -> raise exn)
