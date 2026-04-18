val add_callbacks :
     fn:(int -> Runtime_events.Timestamp.t -> Miou.Trace.event -> unit)
  -> Runtime_events.Callbacks.t
  -> Runtime_events.Callbacks.t
