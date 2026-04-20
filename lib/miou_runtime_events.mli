(** {1 The Miou [Trace] system with [Runtime_events].}

    This module allows you to {i connect} the trace data output by Miou to the
    [Runtime_events] module provided by OCaml using the {!val:reporter}
    function. To ensure that the user has a trace of Miou events, they must call
    [Miou.Trace.set_reporter Miou_runtime_events.reporter]. It should be noted
    that the trace is only emitted if the environment variable [MIOU_TRACE=1]
    exists.

    This module also allows a consumer to be associated with Miou events using
    {!val:add_callbacks}. In this way, the user, using the [Runtime_events]
    module, can obtain Miou events from a program via a {i cursor}. *)

val reporter : Miou.Trace.event -> unit
(** [reporter] is a function to connect the Trace producer to the
    [Runtime_events] producer. If the user would like to emit trace from a Miou
    application, the program must be executed with [MIOU_TRACE=1] and:

    {[
    let () = Miou.Trace.set_reporter Miou_runtime_events.reporter
    ]} *)

val add_callbacks :
     fn:(int -> Runtime_events.Timestamp.t -> Miou.Trace.event -> unit)
  -> Runtime_events.Callbacks.t
  -> Runtime_events.Callbacks.t
(** [add_callbacks ~fn cbs] adds a consumer [fn] for Miou events.

    {[
      let cursor = Runtime_events.create_cursor (Some (cwd, pid)) in
      let cbs = Runtime_events.Callbacks.create () in
      let cbs = Miou_runtime_events.add_callbacks ~fn cbs in
      ...
      let _ = Runtime_events.read_poll cursor cbs None in
      ...
      Runtime_events.free_cursor cursor
    ]} *)
