let src = Logs.Src.create "miou.top"

module Log = (val Logs.src_log src : Logs.LOG)

let to_event = function
  | Miou_runtime_events.Spawn v -> `Spawn v
  | Miou_runtime_events.Cancel v -> `Cancel v
  | Miou_runtime_events.Await v -> `Await v
  | Miou_runtime_events.Resume v -> `Resume v
  | Miou_runtime_events.Cancelled v -> `Cancelled v

let events ~latency_begin ~latency_end stream =
  let queue = Miou.Queue.create () in
  let cbs =
    Runtime_events.Callbacks.create ~runtime_begin:latency_begin
      ~runtime_end:latency_end
      ~lost_events:(fun domain count ->
        Log.warn (fun m -> m "[%d] losts %d events" domain count))
      ()
  in
  let fn ring_id ts ev = Miou.Queue.enqueue queue (ring_id, ts, to_event ev) in
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
      if program_stopped ~program then Miou.Queue.enqueue queue `Stop
      else runtime_events ~sleep ~program queue cursor cbs
  | n ->
      let sleep = Float.max min_sleep (sleep /. 1.2) in
      Miou_unix.sleep sleep;
      runtime_events ~sleep ~program queue cursor cbs

let document ~program handle =
  let cbs, queue = events ~latency_begin ~latency_end in
  let runtime_events =
    Miou.call @@ fun () ->
    let cursor = Runtime_events.create_cursor (Some handle) in
    runtime_events ~sleep:max_sleep ~program queue cursor cbs
  in
  Miou.await_all [ runtime_events ]
