open Miou

let or_raise = function Ok value -> value | Error exn -> raise exn

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
    rd: (Unix.file_descr, unit Miou.syscall) Hashtbl.t
  ; wr: (Unix.file_descr, unit Miou.syscall) Hashtbl.t
  ; sleepers: (Id.t, float * unit Miou.syscall) Hashtbl.t
}

let clean_syscalls dom =
  let fold _ prm = if Miou.is_pending prm then Some prm else None in
  Hashtbl.filter_map_inplace fold dom.rd;
  Hashtbl.filter_map_inplace fold dom.wr;
  let fold _ (until, prm) =
    if Miou.is_pending prm then Some (until, prm) else None
  in
  Hashtbl.filter_map_inplace fold dom.sleepers

let dom =
  let make () =
    {
      rd= Hashtbl.create 0x100
    ; wr= Hashtbl.create 0x100
    ; sleepers= Hashtbl.create 0x100
    }
  in
  let dom = Domain.DLS.new_key make in
  fun () -> Domain.DLS.get dom

let minimum sleepers =
  let fold uid (until, prm) = function
    | Some (_, until', _) when until < until' -> Some (uid, until, prm)
    | Some _ as acc -> acc
    | None -> Some (uid, until, prm)
  in
  Hashtbl.fold fold sleepers None

let sleeper () =
  let dom = dom () in
  match minimum dom.sleepers with
  | Some (uid, _, prm) ->
      let k () = Hashtbl.remove dom.sleepers uid in
      [ Miou.task prm k ]
  | None -> []

let blocking_read fd =
  let dom = dom () in
  let prm = Miou.make (Fun.const ()) in
  Hashtbl.add dom.rd fd prm;
  or_raise (Miou.suspend prm)

let blocking_write fd =
  let dom = dom () in
  let prm = Miou.make (Fun.const ()) in
  Hashtbl.add dom.wr fd prm;
  or_raise (Miou.suspend prm)

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
    ; counter: int Atomic.t
    ; mutex: Mutex.t
    ; uid: int
  }

  let make ?(mutex = Mutex.create ()) () =
    let ic, oc = Unix.pipe ~cloexec:true () in
    let counter = Atomic.make 0 in
    let t = { ic; oc; mutex; counter; uid= gen () } in
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
    let res = Bytes.make 1 '\042' in
    let rec go () =
      match Unix.single_write fd res 0 1 with
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> go ()
      | 0 -> go ()
      | _ -> ()
    in
    go ()

  let wait ~fn t =
    with_lock ~lock:t.mutex (fun () -> Atomic.incr t.counter);
    blocking_read t.ic;
    consume_signal t.ic;
    with_lock ~lock:t.mutex @@ fun () -> Atomic.decr t.counter; fn ()

  let until ~predicate ~fn t =
    Mutex.lock t.mutex;
    while predicate () do
      Atomic.incr t.counter;
      Mutex.unlock t.mutex;
      blocking_read t.ic;
      consume_signal t.ic;
      Mutex.lock t.mutex;
      Atomic.decr t.counter
    done;
    let finally () = Mutex.unlock t.mutex in
    Fun.protect ~finally fn

  let signal t = blocking_write t.oc; produce_signal t.oc; yield ()

  let broadcast t =
    with_lock ~lock:t.mutex @@ fun () ->
    for _ = 0 to Atomic.get t.counter - 1 do
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
        let close fd =
          Unix.close fd;
          Format.printf "%d closed\n%!" (Obj.magic fd)
        in
        let owner = Ownership.own ~finally:close fd in
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

let consume_interrupt interrupt =
  ignore (Unix.read interrupt (Bytes.create 1) 0 1)

let select _domain interrupt () =
  let dom = dom () in
  clean_syscalls dom;
  let rds = Hashtbl.fold (fun fd _ acc -> fd :: acc) dom.rd [] in
  let wrs = Hashtbl.fold (fun fd _ acc -> fd :: acc) dom.wr [] in
  let ts =
    Option.map (fun (_, until, _) -> until) (minimum dom.sleepers) |> function
    | Some ts -> ts
    | None when rds = [] && wrs = [] -> 0.
    | None -> -1.
  in
  let t0 = Unix.gettimeofday () in
  match Unix.select (interrupt :: rds) wrs [] ts with
  | exception Unix.(Unix_error (EINTR, _, _)) -> []
  | [], [], _ ->
      let t1 = Unix.gettimeofday () in
      let syscalls = sleeper () in
      Hashtbl.filter_map_inplace
        (fun _ (until, prm) ->
          let until' = until -. (t1 -. t0) in
          Some (until', prm))
        dom.sleepers;
      syscalls
  | rds, wrs, [] ->
      if List.exists (( = ) interrupt) rds then consume_interrupt interrupt;
      let t1 = Unix.gettimeofday () in
      Hashtbl.filter_map_inplace
        (fun _ (until, prm) ->
          let until' = until -. (t1 -. t0) in
          Some (until', prm))
        dom.sleepers;
      let syscalls = [] in
      let syscalls =
        List.fold_left
          (fun acc fd ->
            if fd <> interrupt then
              let prm = Hashtbl.find dom.rd fd in
              Miou.task prm (fun () -> Hashtbl.remove dom.rd fd) :: acc
            else acc)
          syscalls rds
      in
      let syscalls =
        List.fold_left
          (fun acc fd ->
            let prm = Hashtbl.find dom.wr fd in
            Miou.task prm (fun () -> Hashtbl.remove dom.wr fd) :: acc)
          syscalls wrs
      in
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
  let select () = select domain ic () in
  let t = { Miou.interrupt; select } in
  let close _ = Unix.close ic; Unix.close oc in
  Gc.finalise close t; t

let run ?g ?domains fn = Miou.run ~events ?g ?domains fn

let sleep until =
  let dom = dom () in
  let prm = Miou.make (Fun.const ()) in
  Hashtbl.add dom.sleepers (Miou.uid prm) (until, prm);
  or_raise (Miou.suspend prm)
