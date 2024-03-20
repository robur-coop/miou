let src = Logs.Src.create "tls-miou"

module Log = (val Logs.src_log src : Logs.LOG)

module Make (Flow : Flow.S) = struct
  type error =
    [ `Tls_alert of Tls.Packet.alert_type
    | `Tls_failure of Tls.Engine.failure
    | `Read of Flow.error
    | `Write of Flow.error
    | `Closed ]

  let pp_error ppf = function
    | `Tls_failure failure -> Tls.Engine.pp_failure ppf failure
    | `Tls_alert alert -> Fmt.string ppf (Tls.Packet.alert_type_to_string alert)
    | `Read err -> Flow.pp_error ppf err
    | `Write err -> Flow.pp_error ppf err
    | `Closed -> Fmt.string ppf "Connection closed by peer"

  type state = [ `Active of Tls.Engine.state | `End_of_input | `Error of error ]

  type t = {
      role: [ `Server | `Client ]
    ; flow: Flow.t
    ; mutable state: state
    ; recv: bytes
    ; mutable linger: Cstruct.t option
    ; mutable writer_closed: bool
  }

  let write flow buf =
    match Flow.writev flow.flow [ buf ] with
    | Ok () -> Ok ()
    | Error err ->
        match flow.state with
        | `Active _ ->
            flow.state <- `Error (`Write err);
            Error err
        | _ -> Error err
    [@@ocamlformat "disable"]

  let write_ign flow buf = ignore (Flow.writev flow.flow [ buf ])
  let tls_alert alert = `Error (`Tls_alert alert)
  let tls_fail failure = `Error (`Tls_failure failure)

  let handle flow tls buf =
    match Tls.Engine.handle_tls tls buf with
    | Ok (state', `Response resp, `Data data) ->
        let state' = begin match state' with
            | `Ok tls -> `Active tls
            | `Eof -> `End_of_input
            | `Alert alert -> tls_alert alert end in
        flow.state <- state';
        let _ = Option.map (write flow) resp in
        `Ok data
    | Error (alert, `Response resp) ->
        flow.state <- tls_fail alert;
        match write flow resp with
        | Ok () -> tls_fail alert
        | Error err -> `Error (`Write err)
  [@@ocamlformat "disable"]

  let read_react flow =
    match flow.state with
    | (`End_of_input | `Error _) as v -> v
    | `Active _ ->
        let result =
          Flow.read flow.flow flow.recv ~off:0 ~len:(Bytes.length flow.recv) in
        match flow.state, result with
        | `Active _, Error err -> flow.state <- `Error (`Read err); `Error (`Read err)
        | `Active _, Ok 0 -> flow.state <- `End_of_input; `End_of_input
        | `Active tls, Ok len ->
            handle flow tls (Cstruct.of_bytes flow.recv ~off:0 ~len)
        | `Error e, _ -> `Error e
        | `End_of_input, _ -> `End_of_input
  [@@ocamlformat "disable"]

  let write_out buf ~off ~len flow res =
    let open Cstruct in
    let rlen = length res in
    let n = min len rlen in
    Cstruct.blit_to_bytes res 0 buf off n;
    flow.linger <- (if n < rlen then Some (sub res n (rlen - n)) else None);
    Ok n

  let rec read flow buf ~off ~len =
    match flow.linger with
    | Some res -> write_out buf ~off ~len flow res
    | None -> (
        match read_react flow with
        | `Ok None -> read flow buf ~off ~len
        | `Ok (Some res) -> write_out buf ~off ~len flow res
        | `End_of_input -> Ok 0
        | `Error err -> Error err)

  let rec drain_handshake flow =
    let push_linger flow mcs =
      match (mcs, flow.linger) with
      | None, _ -> ()
      | scs, None -> flow.linger <- scs
      | Some cs, Some l -> flow.linger <- Some (Cstruct.append l cs)
    in
    match flow.state with
    | `Active tls when not (Tls.Engine.handshake_in_progress tls) -> Ok flow
    | _ -> (
        match read_react flow with
        | `Ok cs -> push_linger flow cs; drain_handshake flow
        | `Error err -> Error err
        | `End_of_input -> Error `Closed)

  let close flow =
    match flow.state with
    | `Active tls ->
        let _, buf = Tls.Engine.send_close_notify tls in
        flow.state <- `End_of_input;
        write_ign flow buf;
        Flow.close flow.flow
    | _ -> ()

  let shutdown flow v =
    flow.writer_closed <- true;
    Flow.shutdown flow.flow v

  let client_of_flow conf ?host flow =
    let conf' =
      match host with None -> conf | Some host -> Tls.Config.peer conf host
    in
    let tls, init = Tls.Engine.client conf' in
    let tls_flow =
      {
        role= `Client
      ; flow
      ; state= `Active tls
      ; linger= None
      ; recv= Bytes.create 0x1000
      ; writer_closed= false
      }
    in
    match write tls_flow init with
    | Ok () -> drain_handshake tls_flow
    | Error err -> Error (`Write err)

  let server_of_flow config flow =
    let tls_flow =
      {
        role= `Server
      ; flow
      ; state= `Active (Tls.Engine.server config)
      ; linger= None
      ; recv= Bytes.create 0x1000
      ; writer_closed= false
      }
    in
    drain_handshake tls_flow

  let writev flow bufs =
    if flow.writer_closed then Error `Closed
    else
      match flow.state with
      | `End_of_input -> Error `Closed
      | `Error e -> Error e
      | `Active tls -> (
          match Tls.Engine.send_application_data tls bufs with
          | Some (tls, answer) ->
              flow.state <- `Active tls;
              Result.map_error (fun err -> `Write err) (write flow answer)
          | None -> assert false)

  let write flow buf = writev flow [ buf ]
end
