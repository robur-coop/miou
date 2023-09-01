let src = Logs.Src.create "happy"

module Log = (val Logs.src_log src : Logs.LOG)

let error_msgf fmt = Format.kasprintf (fun msg -> Error (`Msg msg)) fmt

let pp_sockaddr ppf = function
  | Unix.ADDR_UNIX str -> Format.fprintf ppf "<%s>" str
  | Unix.ADDR_INET (inet_addr, port) ->
      Format.fprintf ppf "%s:%u" (Unix.string_of_inet_addr inet_addr) port

let to_sockaddr (ipaddr, port) =
  Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ipaddr, port)

let clock = Mtime_clock.elapsed_ns
let he_timer_interval = Duration.(to_f (of_ms 10))

type state =
  | In_progress
  | Connected of Unix.sockaddr * Unix.file_descr
  | Failed of string

type entry = Happy_eyeballs.id * attempt * [ `host ] Domain_name.t * addr
and attempt = int
and addr = Ipaddr.t * int
and cancel = attempt * Miou_unix.file_descr Miou.t
and action = [ `Connect_ip of state Atomic.t * addr list ]

type t = {
    timeout: int64
  ; mutable cancel_connecting: cancel list Happy_eyeballs.Waiter_map.t
  ; mutable waiters: state Atomic.t Happy_eyeballs.Waiter_map.t
  ; condition: Miou_unix.Cond.t
  ; queue: action Miou.Queue.t
  ; connections: (Miou.Promise.Uid.t, entry) Hashtbl.t
}

let create ~timeout =
  {
    timeout
  ; cancel_connecting= Happy_eyeballs.Waiter_map.empty
  ; waiters= Happy_eyeballs.Waiter_map.empty
  ; condition= Miou_unix.Cond.make ()
  ; queue= Miou.Queue.create ()
  ; connections= Hashtbl.create 0x100
  }

let try_connect addr () =
  let addr = to_sockaddr addr in
  let socket =
    match Unix.domain_of_sockaddr addr with
    | Unix.PF_UNIX -> Fmt.invalid_arg "Invalid address: %a" pp_sockaddr addr
    | Unix.PF_INET -> Miou_unix.tcpv4 ()
    | Unix.PF_INET6 -> Miou_unix.tcpv6 ()
  in
  try
    Log.debug (fun m -> m "connect to %a" pp_sockaddr addr);
    Miou_unix.connect socket addr;
    Miou_unix.transfer socket
  with Unix.Unix_error (err, _, _) ->
    Fmt.failwith "error connecting to nameserver %a: %s" pp_sockaddr addr
      (Unix.error_message err)

let disown fd = Miou_unix.disown fd; Miou_unix.to_file_descr fd
let getpeername fd = try Some (Unix.getpeername fd) with _exn -> None

let connect t ~connections:orphans host id attempt addr =
  let connection = Miou.call ~orphans (try_connect addr) in
  Hashtbl.add t.connections
    (Miou.Promise.uid connection)
    (id, attempt, host, addr);
  let entry = (attempt, connection) in
  t.cancel_connecting <-
    Happy_eyeballs.Waiter_map.update id
      (function None -> Some [ entry ] | Some cs -> Some (entry :: cs))
      t.cancel_connecting

let handle_one_action t ~connections = function
  | Happy_eyeballs.Connect (host, id, attempt, addr) ->
      connect t ~connections host id attempt addr
  | Happy_eyeballs.Connect_failed (host, id, reason) ->
      Log.warn (fun m ->
          m "connection to %a failed: %s" Domain_name.pp host reason);
      let cancel_connecting, others =
        Happy_eyeballs.Waiter_map.find_and_remove id t.cancel_connecting
      in
      t.cancel_connecting <- cancel_connecting;
      List.iter
        (fun (_, prm) -> Miou.cancel prm)
        (Option.value ~default:[] others);
      (* clean waiter *)
      let waiters, waiter =
        Happy_eyeballs.Waiter_map.find_and_remove id t.waiters
      in
      t.waiters <- waiters;
      let msg =
        Fmt.str "Connection to %a failed: %s" Domain_name.pp host reason
      in
      let transition waiter =
        let set = Atomic.compare_and_set waiter In_progress (Failed msg) in
        if not set then begin
          match Atomic.get waiter with
          | Connected (_, fd) ->
              let sockaddr = getpeername fd in
              let fd = Miou_unix.of_file_descr fd in
              Log.debug (fun m ->
                  m "close the file-descriptor of %a (%a): %s" Domain_name.pp
                    host
                    Fmt.(option ~none:(const string "<none>") pp_sockaddr)
                    sockaddr reason);
              Miou_unix.close fd;
              Atomic.set waiter (Failed msg)
          | In_progress -> Atomic.set waiter (Failed msg)
          | Failed _ -> ()
        end
      in
      Option.iter transition waiter
  | Happy_eyeballs.(Resolve_a _ | Resolve_aaaa _) -> ()

let handle_connection t connection =
  let id, attempt, host, addr =
    let value = Hashtbl.find t.connections (Miou.Promise.uid connection) in
    Hashtbl.remove t.connections (Miou.Promise.uid connection);
    value
  in
  match Miou.await connection with
  | Error exn ->
      let msg =
        match exn with
        | Invalid_argument str | Failure str -> str
        | Miou.Cancelled -> "cancelled"
        | exn -> Printexc.to_string exn
      in
      let fold = function
        | None -> None
        | Some cs -> (
            match List.filter (fun (att, _) -> not (att = attempt)) cs with
            | [] -> None
            | cs -> Some cs)
      in
      t.cancel_connecting <-
        Happy_eyeballs.Waiter_map.update id fold t.cancel_connecting;
      Happy_eyeballs.Connection_failed (host, id, addr, msg)
  | Ok fd ->
      let cancel_connecting, others =
        Happy_eyeballs.Waiter_map.find_and_remove id t.cancel_connecting
      in
      t.cancel_connecting <- cancel_connecting;
      List.iter
        (fun (att, prm) -> if att <> attempt then Miou.cancel prm)
        (Option.value ~default:[] others);
      let waiters, waiter =
        Happy_eyeballs.Waiter_map.find_and_remove id t.waiters
      in
      t.waiters <- waiters;
      let transition waiter =
        let addr = to_sockaddr addr in
        let fd = disown fd in
        let connected = Connected (addr, fd) in
        let set = Atomic.compare_and_set waiter In_progress connected in
        if not set then (
          let fd = Miou_unix.of_file_descr fd in
          Log.debug (fun m ->
              m "close the file-descriptor of %a" Domain_name.pp host);
          Miou_unix.close fd)
      in
      Option.iter transition waiter;
      Happy_eyeballs.Connected (host, id, addr)

let handle_connections t he connections actions =
  let rec go he actions =
    match Option.map (handle_connection t) (Miou.care connections) with
    | Some event ->
        let he, actions' = Happy_eyeballs.event he (clock ()) event in
        (* NOTE(dinosaure): prioritise event's actions. *)
        go he (List.append actions' actions)
    | None -> (he, actions)
  in
  go he actions

exception Timeout

let timeout () =
  Miou.call_cc @@ fun () ->
  Miou_unix.sleep he_timer_interval;
  raise Timeout

let await_actions t he =
  Miou.call_cc @@ fun () ->
  let user's_actions =
    Miou_unix.Cond.until
      ~predicate:(fun () -> Miou.Queue.is_empty t.queue)
      ~fn:(fun () -> Miou.Queue.(to_list (transfer t.queue)))
      t.condition
  in
  let fold (he, actions) = function
    | `Connect_ip (waiter, addrs) ->
        let waiters, id = Happy_eyeballs.Waiter_map.register waiter t.waiters in
        t.waiters <- waiters;
        let he, actions' = Happy_eyeballs.connect_ip he (clock ()) ~id addrs in
        (he, List.rev_append actions actions')
  in
  List.fold_left fold (he, []) user's_actions

let await_events t ~connections he =
  Miou.call_cc @@ fun () ->
  let rec go he =
    match handle_connections t he connections [] with
    | he, [] -> Miou.yield (); go he
    | he, actions -> (he, actions)
  in
  go he

let suspend t he ~connections =
  match
    Miou.await_first
      [ timeout (); await_actions t he; await_events t ~connections he ]
  with
  | Error Timeout -> (he, [])
  | Error exn -> raise exn
  | Ok (he, actions) -> (he, actions)

let daemon t =
  let connections = Miou.orphans () in
  let rec go he actions =
    let he, cont, actions' = Happy_eyeballs.timer he (clock ()) in
    match (cont, List.append actions actions') with
    | `Suspend, [] ->
        let he, actions = suspend t he ~connections in
        go he actions
    | _, actions ->
        let he, actions = handle_connections t he connections actions in
        List.iter (handle_one_action ~connections t) actions;
        go he []
  in
  let he = Happy_eyeballs.create ~connect_timeout:t.timeout (clock ()) in
  let he, actions = suspend ~connections t he in
  go he actions

let connect_ip t nss =
  let waiter = Atomic.make In_progress in
  Miou.Queue.enqueue t.queue (`Connect_ip (waiter, nss));
  Miou_unix.Cond.signal t.condition;
  waiter

let to_pairs lst =
  List.map (fun (`Plaintext (ipaddr, port)) -> (ipaddr, port)) lst

let connect_to_nameservers t nameservers =
  let nss = to_pairs nameservers in
  let waiter = connect_ip t nss in
  let rec wait () =
    match Atomic.get waiter with
    | In_progress -> wait (Miou.yield ())
    | Connected (addr, fd) -> (addr, fd)
    | Failed msg -> failwith msg
  in
  let addr, fd = Miou.await_exn (Miou.call_cc wait) in
  (addr, Miou_unix.of_file_descr fd)

let create ~timeout =
  let t = create ~timeout in
  (Miou.call_cc (fun () -> daemon t), t)
