open Miou

type file_descr = {
    fd: Unix.file_descr
  ; owner: Ownership.t
  ; non_blocking: bool
}

let of_file_descr ?(non_blocking = true) ?owner fd =
  let owner =
    match owner with
    | Some owner -> owner
    | None -> Ownership.own ~finally:Unix.close fd
  in
  if non_blocking then Unix.set_nonblock fd else Unix.clear_nonblock fd;
  { fd; owner; non_blocking }

let to_file_descr { fd; _ } = fd
let owner { owner; _ } = owner
let disown { owner; _ } = Miou.Ownership.disown owner

let tcpv4 () =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.set_nonblock fd;
  let owner = Ownership.own ~finally:Unix.close fd in
  { fd; owner; non_blocking= true }

let tcpv6 () =
  let fd = Unix.socket Unix.PF_INET6 Unix.SOCK_STREAM 0 in
  Unix.set_nonblock fd;
  let owner = Ownership.own ~finally:Unix.close fd in
  { fd; owner; non_blocking= true }

let bind_and_listen ?(backlog = 64) { fd; owner; _ } sockaddr =
  Miou.Ownership.check owner;
  Unix.setsockopt fd Unix.SO_REUSEADDR true;
  Unix.setsockopt fd Unix.SO_REUSEPORT true;
  Unix.bind fd sockaddr;
  Unix.listen fd backlog

type unix_scheduler = {
    rd: (Unix.file_descr, unit Miou.syscall list) Hashtbl.t
  ; wr: (Unix.file_descr, unit Miou.syscall list) Hashtbl.t
  ; sleepers: (float * unit Miou.syscall) Miou.Heapq.t
  ; revert: (Miou.uid, Unix.file_descr) Hashtbl.t
}

let clean_syscalls dom uids =
  let clean uid' fd tbl =
    match Hashtbl.find tbl fd with
    | syscalls ->
        let syscalls =
          List.filter (fun syscall -> uid' <> Miou.uid syscall) syscalls
        in
        Hashtbl.replace tbl fd syscalls
    | exception Not_found -> ()
  in
  let clean uid =
    match Hashtbl.find dom.revert uid with
    | fd -> clean uid fd dom.rd; clean uid fd dom.wr
    | exception Not_found -> ()
  in
  List.iter clean uids;
  List.iter (Hashtbl.remove dom.revert) uids

let dom =
  let make () =
    let dummy = (0.0, Miou.make (Fun.const ())) in
    let compare (a, _) (b, _) = Float.compare a b in
    {
      rd= Hashtbl.create 0x100
    ; wr= Hashtbl.create 0x100
    ; sleepers= Heapq.create ~compare ~dummy 0x100
    ; revert= Hashtbl.create 0x100
    }
  in
  let dom = Stdlib.Domain.DLS.new_key make in
  fun () -> Stdlib.Domain.DLS.get dom

(* Sleepers *)

let rec sleepers_to_syscalls ~now syscalls sleepers =
  match Miou.Heapq.minimum sleepers with
  | exception Miou.Heapq.Empty ->
      List.map (fun syscall -> Miou.task syscall (Fun.const ())) syscalls
  | time, syscall ->
      if time <= now then begin
        Miou.Heapq.remove sleepers;
        sleepers_to_syscalls ~now (syscall :: syscalls) sleepers
      end
      else List.map (fun syscall -> Miou.task syscall (Fun.const ())) syscalls

let smallest_sleeper ~now sleepers =
  match Miou.Heapq.minimum sleepers with
  | exception Miou.Heapq.Empty -> None
  | time, _ ->
      let delta = time -. now in
      if delta <= 0. then None else Some delta

let blocking_read fd =
  let dom = dom () in
  let prm = Miou.make (Fun.const ()) in
  let uid = Miou.uid prm in
  Hashtbl.add dom.revert uid fd;
  (try
     let prms = Hashtbl.find dom.rd fd in
     Hashtbl.replace dom.rd fd (prm :: prms)
   with Not_found -> Hashtbl.add dom.rd fd [ prm ]);
  Miou.suspend prm

let blocking_write fd =
  let dom = dom () in
  let prm = Miou.make (Fun.const ()) in
  let uid = Miou.uid prm in
  Hashtbl.add dom.revert uid fd;
  (try
     let prms = Hashtbl.find dom.wr fd in
     Hashtbl.replace dom.wr fd (prm :: prms)
   with Not_found -> Hashtbl.add dom.wr fd [ prm ]);
  Miou.suspend prm

let with_lock ~lock fn =
  Mutex.lock lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock lock) fn

module Cond = struct
  let null = -1

  let gen =
    let value = Atomic.make (null + 1) in
    fun () -> Atomic.fetch_and_add value 1

  type t = {
      ic: Unix.file_descr
    ; oc: Unix.file_descr
    ; mutable counter: int
    ; mutex_predicate: Mutex.t
    ; mutex_counter: Mutex.t
    ; uid: int
  }

  let make ?mutex:(mutex_predicate = Mutex.create ()) () =
    let ic, oc = Unix.pipe ~cloexec:true () in
    let t =
      {
        ic
      ; oc
      ; mutex_predicate
      ; mutex_counter= Mutex.create ()
      ; counter= 0
      ; uid= gen ()
      }
    in
    let close { ic; oc; _ } = Unix.close ic; Unix.close oc in
    Gc.finalise close t; t

  let consume_signal fd =
    let res = Bytes.create 1 in
    let rec go () =
      match Unix.read fd res 0 1 with
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> go ()
      | 0 -> go ()
      | _ -> ()
    in
    go ()

  let produce_signal fd =
    let res = Bytes.make 1 '\000' in
    let rec go () =
      match Unix.single_write fd res 0 1 with
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> go ()
      | 0 -> go ()
      | _ -> ()
    in
    go ()

  let wait t =
    with_lock ~lock:t.mutex_counter (fun () -> t.counter <- t.counter + 1);
    blocking_read t.ic;
    consume_signal t.ic;
    with_lock ~lock:t.mutex_counter (fun () -> t.counter <- t.counter - 1)

  let until ~predicate ~fn t =
    Mutex.lock t.mutex_predicate;
    while predicate () do
      with_lock ~lock:t.mutex_counter (fun () -> t.counter <- t.counter + 1);
      Mutex.unlock t.mutex_predicate;
      blocking_read t.ic;
      consume_signal t.ic;
      Mutex.lock t.mutex_predicate;
      with_lock ~lock:t.mutex_counter (fun () -> t.counter <- t.counter - 1)
    done;
    let finally () = Mutex.unlock t.mutex_predicate in
    Fun.protect ~finally fn

  let signal t =
    with_lock ~lock:t.mutex_counter @@ fun () ->
    if t.counter > 0 then produce_signal t.oc

  let broadcast t =
    with_lock ~lock:t.mutex_counter @@ fun () ->
    for _ = 0 to t.counter - 1 do
      produce_signal t.oc
    done
end

let rec read ({ fd; non_blocking; owner; _ } as file_descr) buf ~off ~len =
  Miou.Ownership.check owner;
  if non_blocking then
    match Unix.read fd buf off len with
    | exception Unix.(Unix_error (EINTR, _, _)) -> read file_descr buf ~off ~len
    | exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
        blocking_read fd;
        read file_descr buf ~off ~len
    | value -> value
  else
    let rec go () =
      match Unix.read fd buf off len with
      | exception Unix.(Unix_error (EINTR, _, _)) -> go ()
      | value -> value
    in
    blocking_read fd; go ()

let rec write ({ fd; non_blocking; owner; _ } as file_descr) str ~off ~len =
  Miou.Ownership.check owner;
  if non_blocking then
    match Unix.write fd (Bytes.unsafe_of_string str) off len with
    | exception Unix.(Unix_error (EINTR, _, _)) ->
        write file_descr str ~off ~len
    | exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
        blocking_write fd;
        write file_descr str ~off ~len
    | len' when len' < len ->
        write file_descr str ~off:(off + len') ~len:(len - len')
    | _ -> ()
  else
    let rec go () =
      match Unix.write fd (Bytes.unsafe_of_string str) off len with
      | exception Unix.(Unix_error (EINTR, _, _)) -> go ()
      | len' when len' < len ->
          write file_descr str ~off:(off + len') ~len:(len - len')
      | _ -> ()
    in
    blocking_write fd; go ()

let rec connect ({ fd; non_blocking; owner; _ } as file_descr) sockaddr =
  Miou.Ownership.check owner;
  if not non_blocking then
    invalid_arg
      "Miouu.connect: we expect a file descriptor in the non-blocking mode";
  match Unix.connect fd sockaddr with
  | () -> ()
  | exception Unix.(Unix_error (EINTR, _, _)) -> connect file_descr sockaddr
  | exception Unix.(Unix_error (EINPROGRESS, _, _)) -> (
      blocking_write fd;
      match Unix.getsockopt_error fd with
      | None -> ()
      | Some err -> raise (Unix.Unix_error (err, "connect", "")))

let transfer ({ owner; _ } as file_descr) = Ownership.transfer owner; file_descr

let rec accept ?cloexec ({ fd; non_blocking; owner; _ } as file_descr) =
  Miou.Ownership.check owner;
  if non_blocking then (
    match Unix.accept ?cloexec fd with
    | exception Unix.(Unix_error (EINTR, _, _)) -> accept ?cloexec file_descr
    | exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
        blocking_read fd; accept ?cloexec file_descr
    | fd, sockaddr ->
        Unix.set_nonblock fd;
        let owner = Ownership.own ~finally:Unix.close fd in
        let file_descr = { fd; owner; non_blocking= true } in
        (file_descr, sockaddr))
  else
    let rec go () =
      match Unix.accept ?cloexec fd with
      | exception Unix.(Unix_error (EINTR, _, _)) -> go ()
      | fd, sockaddr ->
          Unix.set_nonblock fd;
          let owner = Ownership.own ~finally:Unix.close fd in
          let file_descr = { fd; owner; non_blocking= true } in
          (file_descr, sockaddr)
    in
    blocking_read fd; go ()

let close { fd; owner; _ } =
  Miou.Ownership.check owner; Unix.close fd; Ownership.disown owner

let shutdown { fd; owner; _ } v =
  Miou.Ownership.check owner; Unix.shutdown fd v; Ownership.disown owner

let consume_interrupt interrupt =
  ignore (Unix.read interrupt (Bytes.create 1) 0 1)

let of_nano ns = Int64.to_float ns /. 1_000_000_000.

let quanta =
  match Sys.getenv_opt "MIOU_UNIX_QUANTA" with
  | Some str -> (
      try of_nano (Int64.abs (Int64.of_string str))
      with _ -> of_nano 1_000_000L)
  | None -> of_nano 1_000_000L

let transmit_fds syscalls tbl fds =
  let fold acc fd =
    match Hashtbl.find_opt tbl fd with
    | None -> acc
    | Some [] -> Hashtbl.remove tbl fd; acc
    | Some prms ->
        let f prm = Miou.task prm (Fun.const ()) in
        Hashtbl.remove tbl fd;
        List.(rev_append (rev_map f prms) acc)
  in
  List.fold_left fold syscalls fds

let select _domain interrupt cancelled_syscalls =
  let dom = dom () in
  clean_syscalls dom cancelled_syscalls;
  let rds = Hashtbl.fold (fun fd _ acc -> fd :: acc) dom.rd [] in
  let wrs = Hashtbl.fold (fun fd _ acc -> fd :: acc) dom.wr [] in
  let now = Unix.gettimeofday () in
  let ts =
    match smallest_sleeper ~now dom.sleepers with
    | Some ts -> min ts quanta
    | None -> quanta
  in
  Miou.Logs.debug (fun m ->
      m "@[<1>(rds: %d, wrs: %d, sleepers: %d)@]" (Hashtbl.length dom.rd)
        (Hashtbl.length dom.wr)
        (Heapq.length dom.sleepers));
  match Unix.select (interrupt :: rds) wrs [] ts with
  | exception Unix.(Unix_error (EINTR, _, _)) -> []
  | [], [], _ ->
      let now = Unix.gettimeofday () in
      sleepers_to_syscalls ~now [] dom.sleepers
  | rds, wrs, [] ->
      if List.exists (( = ) interrupt) rds then consume_interrupt interrupt;
      let now = Unix.gettimeofday () in
      let syscalls = sleepers_to_syscalls ~now [] dom.sleepers in
      let syscalls = transmit_fds syscalls dom.rd rds in
      let syscalls = transmit_fds syscalls dom.wr wrs in
      syscalls
  | _ -> []

let events domain =
  let ic, oc = Unix.pipe ~cloexec:true () in
  let signal = Bytes.make 1 '\000' in
  let interrupt () =
    while Unix.single_write oc signal 0 1 = 0 do
      ()
    done
  in
  let select = select domain ic in
  let t = { Miou.interrupt; select } in
  let close _ = Unix.close ic; Unix.close oc in
  Gc.finalise close t; t

let run ?g ?domains fn = Miou.run ~events ?g ?domains fn

let sleep until =
  let dom = dom () in
  let prm = Miou.make (Fun.const ()) in
  let now = Unix.gettimeofday () in
  Miou.Heapq.add dom.sleepers (now +. until, prm);
  Miou.suspend prm
