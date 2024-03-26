let src_daemon = Logs.Src.create "happy-daemon"

module Logd = (val Logs.src_log src_daemon : Logs.LOG)

let src_client = Logs.Src.create "happy"

module Logc = (val Logs.src_log src_client : Logs.LOG)

let error_msgf fmt = Format.kasprintf (fun msg -> Error (`Msg msg)) fmt

let pp_sockaddr ppf = function
  | Unix.ADDR_UNIX str -> Format.fprintf ppf "<%s>" str
  | Unix.ADDR_INET (inet_addr, port) ->
      Format.fprintf ppf "%s:%u" (Unix.string_of_inet_addr inet_addr) port

let to_sockaddr (ipaddr, port) =
  Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ipaddr, port)

let clock = Mtime_clock.elapsed_ns

type state =
  | In_progress
  | Connected of (Ipaddr.t * int) * Unix.file_descr
  | Failed of string

type entry = Happy_eyeballs.id * attempt * [ `host ] Domain_name.t * addr
and attempt = int
and addr = Ipaddr.t * int

type cancel = attempt * unit Miou.t

type action =
  [ `Connect_ip of state Atomic.t * addr list
  | `Connect of state Atomic.t * [ `host ] Domain_name.t * int list ]

type connected = [ `Connected of entry * Miou_unix.file_descr ]

type event =
  [ connected
  | `Connection_failed of entry * string
  | `Resolution_v4 of
    [ `host ] Domain_name.t * (Ipaddr.V4.Set.t, [ `Msg of string ]) result
  | `Resolution_v6 of
    [ `host ] Domain_name.t * (Ipaddr.V6.Set.t, [ `Msg of string ]) result ]

and getaddrinfo = {
    getaddrinfo:
      'response 'a.
         'response Dns.Rr_map.key
      -> 'a Domain_name.t
      -> ('response, [ `Msg of string ]) result
}
[@@unboxed]

let dummy =
  let getaddrinfo _ _ = Error (`Msg "Not implemented") in
  { getaddrinfo }

type stack = {
    mutable cancel_connecting: cancel list Happy_eyeballs.Waiter_map.t
  ; mutable waiters: state Atomic.t Happy_eyeballs.Waiter_map.t
  ; condition: Miou.Condition.t
  ; mutex: Miou.Mutex.t
  ; queue: [ action | event ] Miou.Queue.t
  ; mutable getaddrinfo: getaddrinfo
}

let create_stack () =
  {
    cancel_connecting= Happy_eyeballs.Waiter_map.empty
  ; waiters= Happy_eyeballs.Waiter_map.empty
  ; condition= Miou.Condition.create ()
  ; mutex= Miou.Mutex.create ()
  ; queue= Miou.Queue.create ()
  ; getaddrinfo= dummy
  }

let try_connect t ~meta addr () =
  let id, attempt, _, _ = meta in
  let addr = to_sockaddr addr in
  Logd.debug (fun m ->
      m "connect to %a (%d:%d)" pp_sockaddr addr (Obj.magic id) attempt);
  let socket =
    match Unix.domain_of_sockaddr addr with
    | Unix.PF_UNIX -> Fmt.invalid_arg "Invalid address: %a" pp_sockaddr addr
    | Unix.PF_INET -> Miou_unix.tcpv4 ()
    | Unix.PF_INET6 -> Miou_unix.tcpv6 ()
  in
  try
    Miou_unix.connect socket addr;
    Logd.debug (fun m ->
        m "connected to %a (%d:%d)" pp_sockaddr addr (Obj.magic id) attempt);
    Miou.Mutex.protect t.mutex @@ fun () ->
    Miou.Queue.enqueue t.queue (`Connected (meta, socket));
    Miou.Condition.signal t.condition
  with Unix.Unix_error (err, _, _) ->
    Logd.err (fun m ->
        m "error connecting to %a: %s" pp_sockaddr addr (Unix.error_message err));
    Miou_unix.close socket;
    let msg =
      Fmt.str "error connecting to %a: %s" pp_sockaddr addr
        (Unix.error_message err)
    in
    Miou.Mutex.protect t.mutex @@ fun () ->
    Miou.Queue.enqueue t.queue (`Connection_failed (meta, msg));
    Miou.Condition.signal t.condition

let getpeername fd = try Some (Unix.getpeername fd) with _exn -> None

let connect t ~prms:orphans host id attempt addr =
  let meta = (id, attempt, host, addr) in
  Logd.debug (fun m ->
      m "connect to %a (%d:%d)" Domain_name.pp host (Obj.magic id) attempt);
  let prm : unit Miou.t = Miou.call_cc ~orphans (try_connect t ~meta addr) in
  let entry = (attempt, prm) in
  t.cancel_connecting <-
    Happy_eyeballs.Waiter_map.update id
      (function None -> Some [ entry ] | Some cs -> Some (entry :: cs))
      t.cancel_connecting

let handle_one_action t ~prms action =
  match action with
  | Happy_eyeballs.Connect (host, id, attempt, addr) ->
      connect t ~prms host id attempt addr
  | Happy_eyeballs.Connect_failed (host, id, reason) ->
      Logd.warn (fun m ->
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
              Logd.warn (fun m ->
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
  | Happy_eyeballs.Resolve_a host ->
      let _ =
        Miou.call_cc ~orphans:prms @@ fun () ->
        let result =
          match t.getaddrinfo.getaddrinfo Dns.Rr_map.A host with
          | Ok (_ttl, res) -> Ok res
          | Error _ as err -> err
        in
        Miou.Mutex.protect t.mutex @@ fun () ->
        Miou.Queue.enqueue t.queue (`Resolution_v4 (host, result));
        Miou.Condition.signal t.condition
      in
      ()
  | Happy_eyeballs.Resolve_aaaa host ->
      let _ =
        Miou.call_cc ~orphans:prms @@ fun () ->
        let result =
          match t.getaddrinfo.getaddrinfo Dns.Rr_map.Aaaa host with
          | Ok (_ttl, res) -> Ok res
          | Error _ as err -> err
        in
        Miou.Mutex.protect t.mutex @@ fun () ->
        Miou.Queue.enqueue t.queue (`Resolution_v6 (host, result));
        Miou.Condition.signal t.condition
      in
      ()

let to_event t = function
  | `Connection_failed ((id, attempt, host, addr), msg) ->
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
  | `Connected ((id, attempt, host, addr), fd) as event ->
      let cancel_connecting, others =
        Happy_eyeballs.Waiter_map.find_and_remove id t.cancel_connecting
      in
      t.cancel_connecting <- cancel_connecting;
      List.iter
        (fun (att, prm) ->
          if att <> attempt then begin
            Logd.debug (fun m -> m "cancel (%d:%d)" (Obj.magic id) attempt);
            Miou.cancel prm
          end)
        (Option.value ~default:[] others);
      let waiters, waiter =
        Happy_eyeballs.Waiter_map.find_and_remove id t.waiters
      in
      t.waiters <- waiters;
      let () =
        match waiter with
        | None ->
            Format.eprintf ">>> not wait!!!!\n%!";
            Miou.Mutex.protect t.mutex @@ fun () ->
            Miou.Queue.enqueue t.queue event;
            Miou.Condition.signal t.condition
        | Some waiter ->
            let connected = Connected (addr, Miou_unix.to_file_descr fd) in
            let set = Atomic.compare_and_set waiter In_progress connected in
            Logd.debug (fun m -> m "file-descr transmitted? %b" set);
            if not set then Miou_unix.close fd
      in
      Logd.debug (fun m ->
          m "connected to %a (%a) (%d:%d)" Domain_name.pp host pp_sockaddr
            (to_sockaddr addr) (Obj.magic id) attempt);
      Happy_eyeballs.Connected (host, id, addr)
  | `Resolution_v4 (host, Ok ips) ->
      Logd.debug (fun m -> m "%a resolved" Domain_name.pp host);
      Happy_eyeballs.Resolved_a (host, ips)
  | `Resolution_v4 (host, Error (`Msg msg)) ->
      Logd.warn (fun m ->
          m "impossible to resolve %a: %s" Domain_name.pp host msg);
      Happy_eyeballs.Resolved_a_failed (host, msg)
  | `Resolution_v6 (host, Ok ips) ->
      Logd.debug (fun m -> m "%a resolved" Domain_name.pp host);
      Happy_eyeballs.Resolved_aaaa (host, ips)
  | `Resolution_v6 (host, Error (`Msg msg)) ->
      Logd.warn (fun m ->
          m "impossible to resolve %a: %s" Domain_name.pp host msg);
      Happy_eyeballs.Resolved_aaaa_failed (host, msg)

let to_actions t he user's_actions =
  let fold (he, actions) = function
    | `Connect_ip (waiter, addrs) ->
        let waiters, id = Happy_eyeballs.Waiter_map.register waiter t.waiters in
        t.waiters <- waiters;
        let he, actions' = Happy_eyeballs.connect_ip he (clock ()) ~id addrs in
        (he, actions @ actions')
    | `Connect (waiter, host, ports) ->
        let waiters, id = Happy_eyeballs.Waiter_map.register waiter t.waiters in
        t.waiters <- waiters;
        let he, actions' =
          Happy_eyeballs.connect he (clock ()) ~id host ports
        in
        (he, actions @ actions')
  in
  List.fold_left fold (he, []) user's_actions

let await_actions_or_events t () =
  Miou.Mutex.protect t.mutex @@ fun () ->
  while Miou.Queue.is_empty t.queue do
    Miou.Condition.wait t.condition t.mutex
  done

exception Timeout

let with_timeout ~timeout:ts fn =
  let timeout () = Miou_unix.sleep ts; raise Timeout in
  let prm1 = Miou.call_cc timeout in
  let prm0 = Miou.call_cc fn in
  Miou.await_first [ prm0; prm1 ]

let he_timer_interval = Duration.(to_f (of_ms 10))

let suspend t _cont he =
  let timeout = he_timer_interval in
  match with_timeout ~timeout (await_actions_or_events t) with
  | Error Timeout -> (he, [], [])
  | Ok () ->
      let user's_actions_and_events = Miou.Queue.(to_list (transfer t.queue)) in
      Logd.debug (fun m ->
          m "got %d actions or events" (List.length user's_actions_and_events));
      let user's_actions, events =
        List.partition_map
          (function
            | #action as action -> Either.Left action
            | #event as event -> Either.Right event)
          user's_actions_and_events
      in
      Logd.debug (fun m ->
          m "got %d actions and %d events"
            (List.length user's_actions)
            (List.length events));
      let he, actions = to_actions t he user's_actions in
      (he, actions, events)
  | Error exn ->
      Logd.err (fun m ->
          m "Got an unexpected exception (suspend): %s" (Printexc.to_string exn));
      raise exn

let rec clean_up prms =
  match Miou.care prms with
  | Some (Some prm) ->
      let _ = Miou.await prm in
      clean_up prms
  | Some None | None -> Miou.yield ()

let rec go t ~prms he () =
  Logd.debug (fun m -> m "daemon tick");
  clean_up prms;
  let he, cont, actions = Happy_eyeballs.timer he (clock ()) in
  List.iter (handle_one_action ~prms t) actions;
  let he, actions, events = suspend t cont he in
  Logd.debug (fun m ->
      m "got %d action(s) and %d event(s)" (List.length actions)
        (List.length events));
  let he, actions =
    List.fold_left
      (fun (he, actions) event ->
        let he, actions' =
          Happy_eyeballs.event he (clock ()) (to_event t event)
        in
        (he, List.rev_append actions actions'))
      (he, actions) events
  in
  Logd.debug (fun m -> m "daemon handles %d action(s)" (List.length actions));
  List.iter (handle_one_action ~prms t) actions;
  go t ~prms he ()

let launch_daemon ?aaaa_timeout ?connect_delay ?connect_timeout ?resolve_timeout
    ?resolve_retries t () =
  let prms = Miou.orphans () in
  let he =
    Happy_eyeballs.create ?aaaa_timeout ?connect_delay ?connect_timeout
      ?resolve_timeout ?resolve_retries (clock ())
  in
  Miou.call (go t ~prms he)

let _pp_addr ppf (ipaddr, port) = Fmt.pf ppf "%a:%d" Ipaddr.pp ipaddr port

let connect_ip t ips =
  let waiter = Atomic.make In_progress in
  Miou.Mutex.protect t.mutex @@ fun () ->
  Miou.Queue.enqueue t.queue (`Connect_ip (waiter, ips));
  Miou.Condition.signal t.condition;
  waiter

let connect_ip t ips =
  try connect_ip t ips
  with exn ->
    Logc.err (fun m ->
        m "Got an unexpected exception: %S" (Printexc.to_string exn));
    raise exn

let to_pairs lst =
  List.map (fun (`Plaintext (ipaddr, port)) -> (ipaddr, port)) lst

let he_wait_interval = Duration.(to_f (of_ms 10))

let rec wait value =
  Logc.debug (fun m -> m "wait for a connected socket");
  match Atomic.get value with
  | In_progress ->
      Miou_unix.sleep he_wait_interval;
      Miou.yield ();
      wait value
  | Connected (addr, fd) -> (addr, fd)
  | Failed msg -> failwith msg

let connect_to_nameservers t nameservers =
  let nss = to_pairs nameservers in
  let waiter = connect_ip t nss in
  let prm = Miou.call_cc @@ fun () -> wait waiter in
  let addr, fd = Miou.await_exn prm in
  (addr, Miou_unix.of_file_descr fd)

type daemon = unit Miou.t

let stack ?aaaa_timeout ?connect_delay ?connect_timeout ?resolve_timeout
    ?resolve_retries () =
  let v = create_stack () in
  ( launch_daemon ?aaaa_timeout ?connect_delay ?connect_timeout ?resolve_timeout
      ?resolve_retries v ()
  , v )

let inject_resolver ~getaddrinfo stack = stack.getaddrinfo <- getaddrinfo
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
  match t.proto with
  | `Tcp -> (
      try
        let _addr, fd = connect_to_nameservers t.stack t.nameservers in
        Ok (`Tcp, (t.timeout, fd))
      with
      | Failure msg -> Error (`Msg msg)
      | exn ->
          Logc.err (fun m ->
              m "Got unexpected exception: %S" (Printexc.to_string exn));
          raise exn)
  | `Udp -> error_msgf "Invalid protocol"

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
      let len = Miou_unix.read fd buf 0 (Bytes.length buf) in
      Logc.debug (fun m -> m "got %d byte(s) from the resolver" len);
      if len > 0 then process (Cstruct.of_bytes ~off:0 ~len buf)
      else
        Fmt.failwith "End of file reading from resolver (linger: %d byte(s))"
          (Cstruct.length linger)

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
          Logc.debug (fun m -> m "sending a dns packet to resolver");
          Miou_unix.write fd (Cstruct.to_string tx) 0 len;
          let id = Cstruct.BE.get_uint16 tx 2 in
          Logc.debug (fun m -> m "recving a dns packet from resolver");
          let packet = read_loop ~id `Tcp fd in
          (packet, fd)
        in
        match with_timeout ~timeout fn with
        | Ok (packet, _) ->
            Logc.debug (fun m -> m "got a DNS packet from the resolver");
            Ok packet
        | Error Timeout ->
            Logc.warn (fun m -> m "DNS request timeout");
            error_msgf "DNS request timeout"
        | Error (Failure msg) ->
            Logc.warn (fun m -> m "Got a failure: %s" msg);
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

external reraise : exn -> 'a = "%reraise"

let connect_ip t ips =
  let waiter = connect_ip t ips in
  let prm = Miou.call_cc @@ fun () -> wait waiter in
  match Miou.await prm with
  | Ok (addr, fd) -> Ok (addr, Miou_unix.of_file_descr fd)
  | Error (Failure msg) -> Error (`Msg msg)
  | Error exn -> reraise exn

let connect_host t host ports =
  let waiter = Atomic.make In_progress in
  let () =
    Miou.Mutex.protect t.mutex @@ fun () ->
    Miou.Queue.enqueue t.queue (`Connect (waiter, host, ports));
    Miou.Condition.signal t.condition
  in
  let prm = Miou.call_cc @@ fun () -> wait waiter in
  match Miou.await prm with
  | Ok (addr, fd) -> Ok (addr, Miou_unix.of_file_descr fd)
  | Error (Failure msg) -> Error (`Msg msg)
  | Error exn -> reraise exn

let connect_endpoint t str ports =
  match Ipaddr.of_string str with
  | Ok ipaddr -> connect_ip t (List.map (fun port -> (ipaddr, port)) ports)
  | Error _ -> (
      match Result.bind (Domain_name.of_string str) Domain_name.host with
      | Ok domain_name -> connect_host t domain_name ports
      | Error _ -> error_msgf "Invalid endpoint: %S" str)
