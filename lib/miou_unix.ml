module Logs = Miou.Logs.Make (struct
  let src = "unix"
end)

type file_descr = { fd: Unix.file_descr; non_blocking: bool }

let of_file_descr ?(non_blocking = true) fd =
  if non_blocking then Unix.set_nonblock fd else Unix.clear_nonblock fd;
  { fd; non_blocking }

let to_file_descr { fd; _ } = fd

let tcpv4 () =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.set_nonblock fd; { fd; non_blocking= true }

let tcpv6 () =
  let fd = Unix.socket Unix.PF_INET6 Unix.SOCK_STREAM 0 in
  Unix.set_nonblock fd; { fd; non_blocking= true }

let bind_and_listen ?(backlog = 64) { fd; _ } sockaddr =
  Unix.setsockopt fd Unix.SO_REUSEADDR true;
  Unix.setsockopt fd Unix.SO_REUSEPORT true;
  Unix.bind fd sockaddr;
  Unix.listen fd backlog

module File_descrs = Hashtbl.Make (struct
  type t = Unix.file_descr

  let equal a b = Int.equal (Obj.magic a) (Obj.magic b)
  let hash v = Obj.magic v land max_int
end)

type domain = {
    readers: Miou.syscall list File_descrs.t
  ; writers: Miou.syscall list File_descrs.t
  ; sleepers: elt Miou.Heapq.t
  ; revert: (Miou.uid, Unix.file_descr) Hashtbl.t
}

and elt = { time: float; syscall: Miou.syscall; mutable cancelled: bool }

let clean domain uids =
  let clean uid' (fd : Unix.file_descr) tbl =
    match File_descrs.find tbl fd with
    | exception Not_found -> ()
    | syscalls -> (
        match List.filter (fun s -> uid' <> Miou.uid s) syscalls with
        | [] -> File_descrs.remove tbl fd
        | syscalls -> File_descrs.replace tbl fd syscalls)
  in
  let clean uid =
    match Hashtbl.find domain.revert uid with
    | fd ->
        clean uid fd domain.readers;
        clean uid fd domain.writers
    | exception Not_found -> ()
  in
  List.iter clean uids;
  List.iter (Hashtbl.remove domain.revert) uids;
  let clean ({ syscall; _ } as elt) =
    if List.exists (( = ) (Miou.uid syscall)) uids then elt.cancelled <- true
  in
  Miou.Heapq.iter clean domain.sleepers

let domain =
  let make () =
    let dummy = { time= 0.0; syscall= Obj.magic (); cancelled= false } in
    let compare { time= a; _ } { time= b; _ } = Float.compare a b in
    {
      readers= File_descrs.create 0x100
    ; writers= File_descrs.create 0x100
    ; sleepers= Miou.Heapq.create ~compare ~dummy 0x100
    ; revert= Hashtbl.create 0x100
    }
  in
  let key = Stdlib.Domain.DLS.new_key make in
  fun () -> Stdlib.Domain.DLS.get key

let append tbl fd syscall =
  try
    let syscalls = File_descrs.find tbl fd in
    File_descrs.replace tbl fd (syscall :: syscalls)
  with Not_found -> File_descrs.add tbl fd [ syscall ]

let blocking_read fd =
  let syscall = Miou.syscall () in
  let uid = Miou.uid syscall in
  let domain = domain () in
  Hashtbl.replace domain.revert uid fd;
  Logs.debug (fun m -> m "append [%d] as a reader" (Obj.magic fd));
  append domain.readers fd syscall;
  Miou.suspend syscall

let blocking_write fd =
  let syscall = Miou.syscall () in
  let uid = Miou.uid syscall in
  let domain = domain () in
  Hashtbl.replace domain.revert uid fd;
  append domain.writers fd syscall;
  Miou.suspend syscall

let rec read ({ fd; non_blocking } as file_descr) buf off len =
  if non_blocking then
    match Unix.read fd buf off len with
    | exception Unix.(Unix_error (EINTR, _, _)) -> read file_descr buf off len
    | exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
        blocking_read fd;
        read file_descr buf off len
    | value -> value
  else
    let rec go () =
      match Unix.read fd buf off len with
      | exception Unix.(Unix_error (EINTR, _, _)) -> go ()
      | value -> value
    in
    blocking_read fd; go ()

let rec write ({ fd; non_blocking } as file_descr) str off len =
  if non_blocking then
    match Unix.write fd (Bytes.unsafe_of_string str) off len with
    | exception Unix.(Unix_error (EINTR, _, _)) -> write file_descr str off len
    | exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
        blocking_write fd;
        write file_descr str off len
    | len' when len' < len -> write file_descr str (off + len') (len - len')
    | _ -> ()
  else
    let rec go () =
      match Unix.write fd (Bytes.unsafe_of_string str) off len with
      | exception Unix.(Unix_error (EINTR, _, _)) -> go ()
      | len' when len' < len -> write file_descr str (off + len') (len - len')
      | _ -> ()
    in
    blocking_write fd; go ()

let rec accept ?cloexec ({ fd; non_blocking } as file_descr) =
  if non_blocking then (
    match Unix.accept ?cloexec fd with
    | exception Unix.(Unix_error (EINTR, _, _)) -> accept ?cloexec file_descr
    | exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
        blocking_read fd; accept ?cloexec file_descr
    | fd, sockaddr ->
        Unix.set_nonblock fd;
        let file_descr = { fd; non_blocking= true } in
        (file_descr, sockaddr))
  else
    let rec go () =
      match Unix.accept ?cloexec fd with
      | exception Unix.(Unix_error (EINTR, _, _)) -> go ()
      | fd, sockaddr ->
          Unix.set_nonblock fd;
          let file_descr = { fd; non_blocking= true } in
          (file_descr, sockaddr)
    in
    blocking_read fd; go ()

let close { fd; _ } = Unix.close fd

let sleep until =
  let syscall = Miou.syscall () in
  let domain = domain () in
  let elt =
    { time= Unix.gettimeofday () +. until; syscall; cancelled= false }
  in
  Miou.Heapq.add domain.sleepers elt;
  Miou.suspend syscall

let rec sleeper domain =
  match Miou.Heapq.minimum domain.sleepers with
  | exception Miou.Heapq.Empty -> None
  | { cancelled= true; _ } ->
      Miou.Heapq.remove domain.sleepers;
      sleeper domain
  | { time; _ } -> Some time

let in_the_past t = t = 0. || t <= Unix.gettimeofday ()

let rec collect domain signals =
  match Miou.Heapq.minimum domain.sleepers with
  | exception Miou.Heapq.Empty -> signals
  | { cancelled= true; _ } ->
      Miou.Heapq.remove domain.sleepers;
      collect domain signals
  | { time; syscall; _ } when in_the_past time ->
      Miou.Heapq.remove domain.sleepers;
      collect domain (Miou.signal syscall :: signals)
  | _ -> signals

let file_descrs tbl =
  let res = ref [] in
  File_descrs.iter (fun k _ -> res := k :: !res) tbl;
  !res

let transmit_fds signals revert tbl fds =
  let fold acc fd =
    match File_descrs.find tbl fd with
    | [] -> File_descrs.remove tbl fd; acc
    | syscalls ->
        let transmit syscall =
          Hashtbl.remove revert (Miou.uid syscall);
          Miou.signal syscall
        in
        Logs.debug (fun m -> m "delete [%d]" (Obj.magic fd));
        File_descrs.remove tbl fd;
        List.rev_append (List.rev_map transmit syscalls) acc
    | exception Not_found -> acc
  in
  List.fold_left fold signals fds

let interrupted fd fds = List.exists (( = ) fd) fds

let intr fd =
  let buf = Bytes.create 0x100 in
  ignore (Unix.read fd buf 0 (Bytes.length buf))

let select uid interrupt ~block cancelled_syscalls =
  let domain = domain () in
  clean domain cancelled_syscalls;
  let rds = file_descrs domain.readers in
  let wrs = file_descrs domain.writers in
  let now = Unix.gettimeofday () in
  let timeout =
    match sleeper domain with
    | None when block -> -1.0
    | None -> 0.0
    | Some value ->
        let value = value -. now in
        let value = Float.min value 0.01 in
        let value = Float.max value 0.0 in
        value
  in
  Logs.debug (fun m ->
      m "[%a] [readers:%dw, writers:%dw, sleepers:%dw, revert:%dw]"
        Miou.Domain.Uid.pp uid
        Obj.(reachable_words (repr domain.readers))
        Obj.(reachable_words (repr domain.writers))
        Obj.(reachable_words (repr domain.sleepers))
        Obj.(reachable_words (repr domain.revert)));
  match Unix.select (interrupt :: rds) wrs [] timeout with
  | exception Unix.(Unix_error (EINTR, _, _)) -> []
  | [], [], _ -> collect domain []
  | rds, wrs, _ ->
      if interrupted interrupt rds then intr interrupt;
      let signals = collect domain [] in
      let signals = transmit_fds signals domain.revert domain.readers rds in
      let signals = transmit_fds signals domain.revert domain.writers wrs in
      signals

let rec interrupt oc () =
  if Unix.single_write oc signal 0 1 = 0 then interrupt oc ()

and signal = Bytes.make 1 '\000'

let events domain =
  let ic, oc = Unix.pipe ~cloexec:true () in
  let select = select domain ic in
  let t = { Miou.interrupt= interrupt oc; select } in
  let close _ = Unix.close ic; Unix.close oc in
  Gc.finalise close t; t

let run ?g ?domains fn = Miou.run ~events ?g ?domains fn
