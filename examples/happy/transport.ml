open Miou

let src = Logs.Src.create "mdns.transport"

module Log = (val Logs.src_log src : Logs.LOG)

let with_lock ~lock fn =
  Mutex.lock lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock lock) fn

module Box : sig
  type 'a t

  val push : 'a t -> 'a -> bool
  val take : 'a t -> 'a
  val make : unit -> 'a t
end = struct
  type 'a t = { value: 'a option Atomic.t; lock: Mutex.t * Miouu.Cond.t }

  let push t value =
    Mutex.lock (fst t.lock);
    let set = Atomic.compare_and_set t.value None (Some value) in
    Miouu.Cond.broadcast (snd t.lock);
    Mutex.unlock (fst t.lock);
    set

  let take t =
    while
      with_lock ~lock:(fst t.lock) (fun () ->
          Option.is_none (Atomic.get t.value))
    do
      Miouu.Cond.wait ~fn:ignore (snd t.lock)
    done;
    with_lock ~lock:(fst t.lock) (fun () -> Option.get (Atomic.get t.value))

  let make () =
    { value= Atomic.make None; lock= (Mutex.create (), Miouu.Cond.make ()) }
end

module Cond = struct
  type t = Mutex.t * Miouu.Cond.t

  let make () = (Mutex.create (), Miouu.Cond.make ())

  let wait_and_run_until ~p ~fn t =
    while not (with_lock ~lock:(fst t) p) do
      Miouu.Cond.wait ~fn:ignore (snd t)
    done;
    with_lock ~lock:(fst t) fn

  let run_and_broadcast ~fn t =
    Mutex.lock (fst t);
    Fun.protect ~finally:(fun () -> Mutex.unlock (fst t)) @@ fun () ->
    let value = fn () in
    Miouu.Cond.broadcast (snd t);
    value
end

type msg = [ `Msg of string ]
type io_addr = [ `Plaintext of Ipaddr.t * int ]
type connecting = int * Miouu.file_descr * Miouu.file_descr Prm.t
type waiter = ((Ipaddr.t * int) * Miouu.file_descr, msg) result Box.t
type request = Cstruct.t * (Cstruct.t, msg) result Box.t
type +'a io = 'a
type stack = unit

let bind x f = f x
let lift x = x
let open_error_msg = function Ok _ as v -> v | Error (`Msg _) as v -> v
(* let or_raise = function Ok v -> v | Error exn -> raise exn *)

module M = Map.Make (Int)

type daemon = {
    nameservers: io_addr list
  ; proto: Dns.proto
  ; timeout_ns: int64
  ; cancel_connecting: (Happy_eyeballs.id, connecting list) Hashtbl.t
  ; waiters_lock: Mutex.t
  ; mutable waiters: waiter Happy_eyeballs.Waiter_map.t
  ; he_lock: Mutex.t
  ; mutable he: Happy_eyeballs.t
  ; he_orphans: unit Prm.orphans
  ; timer_condition: Cond.t
  ; stop: bool Atomic.t
}

type t = {
    nameservers: io_addr list
  ; proto: Dns.proto
  ; timeout_ns: int64
  ; daemon: daemon
  ; he_prm: unit Prm.t
  ; user_actions: Happy_eyeballs.action Tq.t
  ; mutable requests: request M.t
  ; requests_condition: Cond.t
  ; mutable current: unit Prm.t
  ; current_lock: Mutex.t
}

and context = t

let clock = Mtime_clock.elapsed_ns

let add_cancellation tbl id attempt fd prm =
  match Hashtbl.find_opt tbl id with
  | Some conns -> Hashtbl.replace tbl id ((attempt, fd, prm) :: conns)
  | None -> Hashtbl.add tbl id [ (attempt, fd, prm) ]

let rem_cancellation tbl id attempt =
  let fn id' conns =
    if id = id' then
      match List.filter (fun (att, _, _) -> not (att = attempt)) conns with
      | [] -> None
      | conns -> Some conns
    else Some conns
  in
  Hashtbl.filter_map_inplace fn tbl

let cancel_same_connections tbl id attempt =
  let close (att, _, prm) = if att <> attempt then Prm.cancel prm in
  let fn id' conns =
    if id' = id then (List.iter close conns; None) else Some conns
  in
  Hashtbl.filter_map_inplace fn tbl

let cancel_all_connections tbl id =
  let close (_, _, prm) = Prm.cancel prm in
  let fn id' conns =
    if id = id' then (List.iter close conns; None) else Some conns
  in
  Hashtbl.filter_map_inplace fn tbl

let to_addr (ipaddr, port) =
  Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ipaddr, port)

let handle_one_action t = function
  | Happy_eyeballs.(Resolve_a _ | Resolve_aaaa _) -> None
  | Happy_eyeballs.Connect_failed (_host, id, reason) ->
      cancel_all_connections t.cancel_connecting id;
      let box =
        with_lock ~lock:t.waiters_lock @@ fun () ->
        let waiters, box =
          Happy_eyeballs.Waiter_map.find_and_remove id t.waiters
        in
        t.waiters <- waiters;
        box
      in
      let msg = `Msg reason in
      Option.iter (fun t -> ignore (Box.push t (Error msg))) box;
      None
  | Happy_eyeballs.Connect (host, id, attempt, addr) -> (
      let fd = Miouu.tcpv4 () in
      let prm =
        Prm.call_cc ~give:[ Miouu.owner fd ] @@ fun () ->
        Miouu.connect fd (to_addr addr);
        Miouu.transfer fd
      in
      add_cancellation t.cancel_connecting id attempt fd prm;
      match Prm.await prm with
      | Ok fd ->
          cancel_same_connections t.cancel_connecting id attempt;
          let box =
            with_lock ~lock:t.waiters_lock @@ fun () ->
            let waiters, box =
              Happy_eyeballs.Waiter_map.find_and_remove id t.waiters
            in
            t.waiters <- waiters;
            box
          in
          begin
            match
              Option.map
                (fun t ->
                  Miouu.disown fd;
                  Box.push t (Ok (addr, fd)))
                box
            with
            | Some true -> ()
            | Some false | None -> Miouu.close fd
          end;
          Some (Happy_eyeballs.Connected (host, id, addr))
      | Error Prm.Cancelled -> None
      | Error exn ->
          rem_cancellation t.cancel_connecting id attempt;
          let err = Fmt.str "Got an exception: %S" (Printexc.to_string exn) in
          Some (Happy_eyeballs.Connection_failed (host, id, addr, err)))

let rec handle_action t action =
  match handle_one_action t action with
  | None -> ()
  | Some event ->
      let actions =
        with_lock ~lock:t.he_lock @@ fun () ->
        let he, actions = Happy_eyeballs.event t.he (clock ()) event in
        t.he <- he;
        actions
      in
      List.iter (handle_action t) actions

let handle_timer_actions t =
  List.iter @@ fun action ->
  ignore (Prm.call_cc ~orphans:t.he_orphans @@ fun () -> handle_action t action)

let rec clean orphans =
  match Prm.care orphans with
  | None -> ()
  | Some prm -> (
      match Prm.await prm with
      | Ok () -> clean orphans
      | Error _exn -> clean orphans)

let rec add_user's_actions actions q =
  match Tq.dequeue q with
  | action -> add_user's_actions (action :: actions) q
  | exception Tq.Empty -> actions

let happy_eyeballs_daemon ~user's_actions t =
  let rec go () =
    let () = Miou.yield () and () = clean t.he_orphans in
    let cont, actions =
      with_lock ~lock:t.he_lock @@ fun () ->
      let he, cont, actions = Happy_eyeballs.timer t.he (clock ()) in
      t.he <- he;
      (cont, actions)
    in
    let actions = add_user's_actions actions user's_actions in
    handle_timer_actions t actions;
    match cont with
    | `Suspend ->
        let p () = (not (Tq.is_empty user's_actions)) || Atomic.get t.stop in
        let stop =
          Cond.wait_and_run_until
            ~fn:(fun () -> Atomic.get t.stop)
            ~p t.timer_condition
        in
        if not stop then go ()
    | `Act -> go ()
  in
  go

let kill_happy_eyeballs_daemon t =
  let fn () = Atomic.set t.stop true in
  Cond.run_and_broadcast ~fn t.timer_condition

let query_one fd data =
  match fd with
  | `Plain fd ->
      let str = Cstruct.to_string data in
      Miouu.write fd str ~off:0 ~len:(String.length str)

let rec wrrd ?(linger = Cstruct.empty) t fd =
  let process cs =
    let rec handle_data data =
      let cs_len = Cstruct.length data in
      if cs_len > 2 then
        let len = Cstruct.BE.get_uint16 data 0 in
        if cs_len - 2 >= len then (
          let packet, rest =
            if cs_len - 2 = len then (data, Cstruct.empty)
            else Cstruct.split data (len + 2)
          in
          let id = Cstruct.BE.get_uint16 packet 2 in
          with_lock ~lock:(fst t.requests_condition) (fun () ->
              match M.find_opt id t.requests with
              | Some (_, box) -> ignore (Box.push box (Ok packet))
              | None -> ());
          handle_data rest)
        else Some data
      else Some data
    in
    let data =
      if Cstruct.length linger = 0 then cs else Cstruct.append linger cs
    in
    handle_data data
  in
  let p () = not (M.is_empty t.requests) in
  let fn () = M.fold (fun _ (cs, _) acc -> cs :: acc) t.requests [] in
  let txs = Cond.wait_and_run_until ~p ~fn t.requests_condition in
  match fd with
  | `Plain fd -> (
      let prm =
        Prm.call_cc ~give:[ Miouu.owner fd ] @@ fun () ->
        List.iter (query_one (`Plain fd)) txs;
        let buf = Bytes.create 0x1000 in
        let len = Miouu.read fd buf ~off:0 ~len:0x1000 in
        if len > 0 then
          let res = process (Cstruct.of_bytes buf ~off:0 ~len) in
          (Miouu.transfer fd, res)
        else (Miouu.transfer fd, None)
      in
      let res = Prm.await prm in
      match res with
      | Ok (fd, Some linger) -> wrrd ~linger t (`Plain fd)
      | Ok (fd, None) -> Miouu.close fd
      | Error _exn -> Miouu.disown fd)

let send_user's_actions t actions =
  let fn () = List.iter (Tq.enqueue t.user_actions) actions in
  Cond.run_and_broadcast ~fn t.daemon.timer_condition

let to_pairs : [ `Plaintext of Ipaddr.t * int ] list -> _ =
  List.map (function `Plaintext (ip, port) -> (ip, port))

let find_ns ns (addr, port) =
  List.find
    (function `Plaintext (ip, p) -> Ipaddr.compare ip addr = 0 && p = port)
    ns

let rec connect_ns_and_do_requests (t : t) nameservers =
  let box = Box.make () in
  let ns = to_pairs nameservers in
  let id =
    with_lock ~lock:t.daemon.waiters_lock @@ fun () ->
    let waiters, id = Happy_eyeballs.Waiter_map.register box t.daemon.waiters in
    t.daemon.waiters <- waiters;
    id
  in
  let actions =
    with_lock ~lock:t.daemon.he_lock (fun () ->
        let he, actions =
          Happy_eyeballs.connect_ip t.daemon.he (clock ()) ~id ns
        in
        t.daemon.he <- he;
        actions)
  in
  send_user's_actions t actions;
  match Box.take box with
  | Error (`Msg _msg) -> ()
  | Ok (addr, fd) -> (
      match find_ns t.nameservers addr with
      | `Plaintext _ ->
          wrrd t (`Plain fd);
          let no_requests =
            with_lock ~lock:(fst t.requests_condition) @@ fun () ->
            M.is_empty t.requests
          in
          if not no_requests then connect_ns_and_do_requests t t.nameservers)

exception Timeout

let with_timeout ns fn =
  let p0 = Prm.call_cc fn in
  let p1 =
    Prm.call_cc @@ fun () ->
    Miouu.sleep (Duration.to_f ns);
    raise Timeout
  in
  match Prm.await_first [ p0; p1 ] with
  | Ok _ as v -> v
  | Error Timeout -> Error (`Msg "DNS request timeout")
  | Error exn -> raise exn

let send_recv t tx =
  if Cstruct.length tx > 4 then (
    let id = Cstruct.BE.get_uint16 tx 2 in
    Result.join
    @@ with_timeout t.timeout_ns
    @@ fun () ->
    let fn () =
      let box = Box.make () in
      t.requests <- M.add id (tx, box) t.requests;
      box
    in
    let box = Cond.run_and_broadcast ~fn t.requests_condition in
    let value = Box.take box in
    with_lock ~lock:(fst t.requests_condition) (fun () ->
        t.requests <- M.remove id t.requests);
    open_error_msg value)
  else Error (`Msg "Invalid DNS packet")

let close _ = ()
let nameservers { proto; nameservers; _ } = (proto, nameservers)
let rng = Mirage_crypto_rng.generate ?g:None

let kill t =
  kill_happy_eyeballs_daemon t.daemon;
  with_lock ~lock:t.current_lock @@ fun () ->
  Prm.cancel t.current; Prm.await t.he_prm

let create ?nameservers ~timeout () =
  let proto, nameservers =
    match nameservers with
    | None -> (`Tcp, [ `Plaintext (Ipaddr.of_string_exn "8.8.8.8", 53) ])
    | Some ns -> ns
  in
  let daemon =
    {
      nameservers
    ; proto
    ; timeout_ns= timeout
    ; cancel_connecting= Hashtbl.create 0x10
    ; waiters_lock= Mutex.create ()
    ; waiters= Happy_eyeballs.Waiter_map.empty
    ; he_lock= Mutex.create ()
    ; he= Happy_eyeballs.create ~connect_timeout:timeout (clock ())
    ; he_orphans= Prm.orphans ()
    ; timer_condition= Cond.make ()
    ; stop= Atomic.make false
    }
  in
  let user's_actions = Tq.make () in
  let he_prm = Prm.call (happy_eyeballs_daemon ~user's_actions daemon) in
  let current = Prm.call_cc (Fun.const ()) and () = yield () in
  {
    nameservers
  ; proto
  ; timeout_ns= timeout
  ; he_prm
  ; user_actions= user's_actions
  ; requests= M.empty
  ; requests_condition= Cond.make ()
  ; current
  ; current_lock= Mutex.create ()
  ; daemon
  }

let connect t =
  let state = with_lock ~lock:t.current_lock @@ fun () -> Prm.state t.current in
  match state with
  | Prm.Pending -> Ok (`Tcp, t)
  | _ ->
      with_lock ~lock:t.current_lock @@ fun () ->
      ignore (Prm.await t.current);
      let prm =
        Prm.call @@ fun () -> connect_ns_and_do_requests t t.nameservers
      in
      t.current <- prm;
      Ok (`Tcp, t)
