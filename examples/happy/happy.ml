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

type stack = {
    mutable cancel_connecting: cancel list Happy_eyeballs.Waiter_map.t
  ; mutable waiters: state Atomic.t Happy_eyeballs.Waiter_map.t
  ; condition: Miou_unix.Cond.t
  ; queue: action Miou.Queue.t
  ; connections: (Miou.Promise.Uid.t, entry) Hashtbl.t
}

let create_daemon () =
  {
    cancel_connecting= Happy_eyeballs.Waiter_map.empty
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

let connect t ~conns:orphans host id attempt addr =
  let connection = Miou.call ~orphans (try_connect addr) in
  Hashtbl.add t.connections
    (Miou.Promise.uid connection)
    (id, attempt, host, addr);
  let entry = (attempt, connection) in
  t.cancel_connecting <-
    Happy_eyeballs.Waiter_map.update id
      (function None -> Some [ entry ] | Some cs -> Some (entry :: cs))
      t.cancel_connecting

let handle_one_action t ~conns = function
  | Happy_eyeballs.Connect (host, id, attempt, addr) ->
      connect t ~conns host id attempt addr
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
              Log.warn (fun m ->
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
        Log.debug (fun m -> m "disown the file-descr of %a" Domain_name.pp host);
        let fd = disown fd in
        let connected = Connected (addr, fd) in
        let set = Atomic.compare_and_set waiter In_progress connected in
        Log.debug (fun m ->
            m "transfer the file-descriptor for %a" Domain_name.pp host);
        if not set then (
          let fd = Miou_unix.of_file_descr fd in
          Log.warn (fun m ->
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

let await_actions t he =
  Miou.call_cc @@ fun () ->
  let user's_actions =
    Miou_unix.Cond.until
      ~predicate:(fun () ->
        Log.debug (fun m ->
            m "got an user's action? %b"
              (Fun.negate Miou.Queue.is_empty t.queue));
        Miou.Queue.is_empty t.queue)
      ~fn:(fun () -> Miou.Queue.(to_list (transfer t.queue)))
      t.condition
  in
  Log.debug (fun m -> m "got %d action(s)" (List.length user's_actions));
  let fold (he, actions) = function
    | `Connect_ip (waiter, addrs) ->
        let waiters, id = Happy_eyeballs.Waiter_map.register waiter t.waiters in
        t.waiters <- waiters;
        let he, actions' = Happy_eyeballs.connect_ip he (clock ()) ~id addrs in
        (he, List.rev_append actions actions')
  in
  List.fold_left fold (he, []) user's_actions

let await_events t he ~conns =
  Miou.call_cc @@ fun () ->
  let rec go he =
    match handle_connections t he conns [] with
    | he, [] -> Miou.yield (); go he
    | he, actions -> (he, actions)
  in
  go he

let suspend t he ~conns =
  let timeout he =
    Miou.call_cc @@ fun () ->
    Miou_unix.sleep he_timer_interval;
    (he, [])
  in
  let prms = [ timeout he; await_actions t he; await_events t he ~conns ] in
  match Miou.await_first prms with
  | Error exn -> raise exn
  | Ok (he, actions) -> (he, actions)

let launch_daemon ~timeout:connect_timeout t () =
  let conns = Miou.orphans () in
  let rec go he () =
    let he, cont, actions = Happy_eyeballs.timer he (clock ()) in
    match (cont, actions) with
    | `Suspend, [] ->
        Log.debug (fun m -> m "the daemon is suspended to a new event");
        let he, actions' = suspend t he ~conns in
        List.iter (handle_one_action ~conns t) (List.append actions actions');
        go he ()
    | _, actions ->
        let he, actions = handle_connections t he conns actions in
        List.iter (handle_one_action ~conns t) actions;
        go he ()
  in
  let he = Happy_eyeballs.create ~connect_timeout (clock ()) in
  Log.debug (fun m -> m "the daemon is launched");
  let he, actions = suspend ~conns t he in
  Log.debug (fun m -> m "consume %d action(s)" (List.length actions));
  List.iter (handle_one_action ~conns t) actions;
  go he ()

let connect_ip t nss =
  let waiter = Atomic.make In_progress in
  Miou.Queue.enqueue t.queue (`Connect_ip (waiter, nss));
  Miou_unix.Cond.signal t.condition;
  Log.debug (fun m -> m "the daemon was signaled about another user's action");
  waiter

let to_pairs lst =
  List.map (fun (`Plaintext (ipaddr, port)) -> (ipaddr, port)) lst

let connect_to_nameservers t nameservers =
  let nss = to_pairs nameservers in
  let waiter = connect_ip t nss in
  let rec wait () =
    match Atomic.get waiter with
    | In_progress ->
        Log.debug (fun m -> m "waiting for a file-descr");
        wait (Miou.yield ())
    | Connected (addr, fd) -> (addr, fd)
    | Failed msg -> failwith msg
  in
  let addr, fd = Miou.await_exn (Miou.call_cc wait) in
  (addr, Miou_unix.of_file_descr fd)

type daemon = unit Miou.t

let daemon ~timeout =
  let v = create_daemon () in
  (Miou.call (launch_daemon ~timeout v), v)

let kill = Miou.cancel

type +'a io = 'a
type io_addr = [ `Plaintext of Ipaddr.t * int ]

type t = {
    nameservers: io_addr list
  ; proto: Dns.proto
  ; timeout: float
  ; stack: stack
}

type context = float * Miou_unix.file_descr

let nameservers { nameservers; proto; _ } = (proto, nameservers)
let bind x f = f x
let lift = Fun.id
let rng = Mirage_crypto_rng.generate ?g:None

let connect t =
  try
    let _addr, fd = connect_to_nameservers t.stack t.nameservers in
    Ok (`Tcp, (t.timeout, fd))
  with Failure msg -> Error (`Msg msg)

let rec read_loop ?(linger = Cstruct.empty) ~id proto fd =
  let process rx =
    let rec handle_data ({ Cstruct.len= rx_len; _ } as rx) =
      if rx_len > 2 then
        let len = Cstruct.BE.get_uint16 rx 0 in
        if rx_len - 2 >= len then
          let packet, rest =
            if rx_len - 2 = len then (rx, Cstruct.empty)
            else Cstruct.split rx (len + 2)
          in
          let id' = Cstruct.BE.get_uint16 packet 2 in
          if id = id' then packet else handle_data rest
        else read_loop ~linger:rx ~id proto fd
      else read_loop ~linger:rx ~id proto fd
    in
    let rx =
      if Cstruct.length linger = 0 then rx else Cstruct.append linger rx
    in
    handle_data rx
  in
  match proto with
  | `Tcp ->
      let buf = Bytes.create 0x10 in
      let len = Miou_unix.read fd ~off:0 ~len:(Bytes.length buf) buf in
      Log.debug (fun m -> m "got %d byte(s) from the resolver" len);
      if len > 0 then process (Cstruct.of_bytes ~off:0 ~len buf)
      else failwith "End of file reading from resolver"

exception Timeout

let with_timeout ~timeout ?give fn =
  let timeout () = Miou_unix.sleep timeout; raise Timeout in
  Miou.await_first [ Miou.call_cc timeout; Miou.call_cc ?give fn ]

external happy_translate_so_type : int -> Unix.socket_type
  = "happy_translate_so_type"

let type_of_socket fd =
  let fd = Miou_unix.to_file_descr fd in
  let ty = Unix.getsockopt_int fd Unix.SO_TYPE in
  happy_translate_so_type ty

let send_recv (timeout, fd) ({ Cstruct.len; _ } as tx) =
  if len > 4 then begin
    match type_of_socket fd with
    | Unix.SOCK_STREAM -> (
        let fn () =
          Log.debug (fun m -> m "send a packet to resolver");
          Miou_unix.write fd ~off:0 ~len (Cstruct.to_bytes tx);
          let id = Cstruct.BE.get_uint16 tx 2 in
          Log.debug (fun m -> m "recv a packet from resolver");
          let packet = read_loop ~id `Tcp fd in
          (packet, Miou_unix.transfer fd)
        in
        match with_timeout ~timeout ~give:[ Miou_unix.owner fd ] fn with
        | Ok (packet, _) ->
            Log.debug (fun m -> m "Got a DNS packet from the resolver");
            Ok packet
        | Error Timeout ->
            Log.warn (fun m -> m "DNS request timeout");
            error_msgf "DNS request timeout"
        | Error (Failure msg) ->
            Log.warn (fun m -> m "Got a failure: %s" msg);
            Error (`Msg msg)
        | Error exn ->
            error_msgf "Got an unexpected exception: %S"
              (Printexc.to_string exn))
    | _ -> error_msgf "Invalid type of file-descriptor"
  end
  else error_msgf "Invalid context (data length <= 4)"

let close (_, fd) = Miou_unix.close fd
let of_ns ns = Int64.to_float ns /. 1_000_000_000.

let create ?nameservers ~timeout stack =
  let proto, nameservers =
    match nameservers with
    | None -> (`Tcp, [ `Plaintext (Ipaddr.of_string_exn "8.8.8.8", 53) ])
    | Some (a, nss) -> (a, nss)
  in
  { nameservers; proto; timeout= of_ns timeout; stack }
