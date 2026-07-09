module Bitv = Miou_bitv

module Logs = Miou.Logs.Make (struct
  let src = "unix"
end)

type kind = STREAM | DGRAM
type file_descr = { fd: Unix.file_descr; kind: kind option; non_blocking: bool }

let is_dgram { kind; _ } = match kind with Some DGRAM -> true | _ -> false

let of_file_descr ?(non_blocking = true) fd =
  if non_blocking then Unix.set_nonblock fd else Unix.clear_nonblock fd;
  match Unix.getsockopt_int fd Unix.SO_TYPE with
  | 1 -> { fd; kind= Some STREAM; non_blocking }
  | 2 -> { fd; kind= Some DGRAM; non_blocking }
  | _ ->
      failwith
        "Miou_unix.of_file_descr: invalid file-descriptor (neither TCP nor UDP)"
  | exception _exn -> { fd; kind= None; non_blocking }

let to_file_descr { fd; _ } = fd

let nonblocking_stream fam =
  let open Unix in
  let fd = socket fam SOCK_STREAM 0 in
  set_nonblock fd;
  { fd; non_blocking= true; kind= Some STREAM }

let nonblocking_dgram fam =
  let open Unix in
  let fd = socket fam SOCK_DGRAM 0 in
  set_nonblock fd;
  { fd; non_blocking= true; kind= Some DGRAM }

let unix_socket () = nonblocking_stream Unix.PF_UNIX
let tcpv4 () = nonblocking_stream Unix.PF_INET
let tcpv6 () = nonblocking_stream Unix.PF_INET6
let udpv4 () = nonblocking_dgram Unix.PF_INET
let udpv6 () = nonblocking_dgram Unix.PF_INET6

let bind_and_listen ?(backlog = 64) ?(reuseaddr = true) ?(reuseport = true)
    ({ fd; _ } as file_descr) sockaddr =
  let open Unix in
  setsockopt fd SO_REUSEADDR reuseaddr;
  setsockopt fd SO_REUSEPORT reuseport;
  bind fd sockaddr;
  (* NOTE(dinosaure): [listen] is meaningless (and fails with [EOPNOTSUPP]) on a
     datagram socket. We still allow [bind_and_listen] to bind such a socket so
     that UDP servers can share the same entry-point. *)
  if not (is_dgram file_descr) then listen fd backlog

module File_descrs = struct
  type 'a t = { mutable contents: (Unix.file_descr * 'a) list }

  let find tbl fd = List.assq fd tbl.contents

  let remove tbl fd =
    let contents =
      List.fold_left
        (fun acc (k, v) -> if k == fd then acc else (k, v) :: acc)
        [] tbl.contents
    in
    tbl.contents <- contents

  let iter f tbl = List.iter (fun (k, v) -> f k v) tbl.contents

  let replace tbl fd v' =
    let contents =
      List.fold_left
        (fun acc (k, v) -> if k == fd then (k, v') :: acc else (k, v) :: acc)
        [] tbl.contents
    in
    tbl.contents <- contents

  let add tbl k v = tbl.contents <- (k, v) :: tbl.contents
  let clear tbl = tbl.contents <- []
  let create _ = { contents= [] }
end

type elt = { time: float; syscall: Miou.syscall; mutable cancelled: bool }

module Heapq = Miou.Pqueue.Make (struct
  type t = elt

  let dummy = { time= 0.0; syscall= Obj.magic (); cancelled= false }
  let compare { time= a; _ } { time= b; _ } = Float.compare a b
end)

type domain = {
    readers: Miou.syscall list File_descrs.t
  ; writers: Miou.syscall list File_descrs.t
  ; sleepers: Heapq.t
  ; revert: (Miou.uid, Unix.file_descr) Hashtbl.t
}

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
  Heapq.iter clean domain.sleepers

let rec drop_heapq heapq =
  try Heapq.delete_min_exn heapq; drop_heapq heapq with _ -> ()

let domain =
  let rec split_from_parent v =
    File_descrs.clear v.readers;
    File_descrs.clear v.writers;
    drop_heapq v.sleepers;
    Hashtbl.clear v.revert;
    make ()
  and make () =
    {
      readers= File_descrs.create 0x100
    ; writers= File_descrs.create 0x100
    ; sleepers= Heapq.create ()
    ; revert= Hashtbl.create 0x100
    }
  in
  let key = Stdlib.Domain.DLS.new_key ~split_from_parent make in
  fun () -> Stdlib.Domain.DLS.get key

let append tbl fd syscall =
  try
    let syscalls = File_descrs.find tbl fd in
    File_descrs.replace tbl fd (syscall :: syscalls)
  with Not_found -> File_descrs.add tbl fd [ syscall ]

let blocking_read ?(name = "read") fd =
  let syscall = Miou.syscall ~name () in
  let uid = Miou.uid syscall in
  let domain = domain () in
  let fn () =
    Hashtbl.replace domain.revert uid fd;
    append domain.readers fd syscall
  in
  Miou.suspend ~fn syscall

let blocking_write ?(name = "write") fd =
  let syscall = Miou.syscall ~name () in
  let uid = Miou.uid syscall in
  let domain = domain () in
  let fn () =
    Hashtbl.replace domain.revert uid fd;
    append domain.writers fd syscall
  in
  Miou.suspend ~fn syscall

let rec unsafe_read ({ fd; non_blocking; _ } as file_descr) off len buf =
  if non_blocking then
    match Unix.read fd buf off len with
    | exception Unix.(Unix_error (EINTR, _, _)) ->
        unsafe_read file_descr off len buf
    | exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
        blocking_read fd;
        unsafe_read file_descr off len buf
    | value -> value
  else
    let rec go () =
      match Unix.read fd buf off len with
      | exception Unix.(Unix_error (EINTR, _, _)) -> go ()
      | value -> value
    in
    blocking_read fd; go ()

let read file_descr ?(off = 0) ?len buf =
  let len = match len with None -> Bytes.length buf - off | Some len -> len in
  if off < 0 || len < 0 || off > Bytes.length buf - len then
    invalid_arg "Miou_unix.read";
  if is_dgram file_descr then invalid_arg "Miou_unix.read: invalid file-descr";
  unsafe_read file_descr off len buf

let rec unsafe_recvfrom ({ fd; non_blocking; _ } as file_descr) off len buf
    flags =
  if non_blocking then
    match Unix.recvfrom fd buf off len flags with
    | exception Unix.(Unix_error (EINTR, _, _)) ->
        unsafe_recvfrom file_descr off len buf flags
    | exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
        blocking_read fd;
        unsafe_recvfrom file_descr off len buf flags
    | value -> value
  else
    let rec go () =
      match Unix.recvfrom fd buf off len flags with
      | exception Unix.(Unix_error (EINTR, _, _)) -> go ()
      | value -> value
    in
    blocking_read fd; go ()

let recvfrom file_descr ?(off = 0) ?len buf flags =
  let len = match len with None -> Bytes.length buf - off | Some len -> len in
  if off < 0 || len < 0 || off > Bytes.length buf - len then
    invalid_arg "Miou_unix.recvfrom";
  if not (is_dgram file_descr) then
    invalid_arg "Miou_unix.recvfrom: invalid file-descr";
  unsafe_recvfrom file_descr off len buf flags

let rec really_read_go file_descr off len buf =
  let len' = unsafe_read file_descr off len buf in
  if len' == 0 then raise End_of_file
  else if len - len' > 0 then
    really_read_go file_descr (off + len') (len - len') buf

let really_read file_descr ?(off = 0) ?len buf =
  let len = match len with None -> Bytes.length buf - off | Some len -> len in
  if off < 0 || len < 0 || off > Bytes.length buf - len then
    invalid_arg "Miou_unix.really_read";
  if is_dgram file_descr then invalid_arg "Miou_unix.read: invalid file-descr";
  if len > 0 then really_read_go file_descr off len buf

let rec unsafe_write ({ fd; non_blocking; _ } as file_descr) off len str =
  if non_blocking then
    match Unix.write fd (Bytes.unsafe_of_string str) off len with
    | exception Unix.(Unix_error (EINTR, _, _)) ->
        unsafe_write file_descr off len str
    | exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
        blocking_write fd;
        unsafe_write file_descr off len str
    | len' when len' < len ->
        unsafe_write file_descr (off + len') (len - len') str
    | _ -> ()
  else
    let rec go () =
      match Unix.write fd (Bytes.unsafe_of_string str) off len with
      | exception Unix.(Unix_error (EINTR, _, _)) -> go ()
      | len' when len' < len ->
          unsafe_write file_descr (off + len') (len - len') str
      | _ -> ()
    in
    blocking_write fd; go ()

let write file_descr ?(off = 0) ?len str =
  let len =
    match len with None -> String.length str - off | Some len -> len
  in
  if off < 0 || len < 0 || off > String.length str - len then
    invalid_arg "Miou_unix.write";
  if is_dgram file_descr then invalid_arg "Miou_unix.write: invalid file-descr";
  unsafe_write file_descr off len str

let rec unsafe_sendto ({ fd; non_blocking; _ } as file_descr) off len str flags
    sockaddr =
  if non_blocking then
    match Unix.sendto_substring fd str off len flags sockaddr with
    | exception Unix.(Unix_error (EINTR, _, _)) ->
        unsafe_sendto file_descr off len str flags sockaddr
    | exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
        blocking_write fd;
        unsafe_sendto file_descr off len str flags sockaddr
    | value -> value
  else
    let rec go () =
      match Unix.sendto_substring fd str off len flags sockaddr with
      | exception Unix.(Unix_error (EINTR, _, _)) -> go ()
      | value -> value
    in
    blocking_write fd; go ()

let sendto file_descr ?(off = 0) ?len str flags sockaddr =
  let len =
    match len with None -> String.length str - off | Some len -> len
  in
  if off < 0 || len < 0 || off > String.length str - len then
    invalid_arg "Miou_unix.sendto";
  if not (is_dgram file_descr) then
    invalid_arg "Miou_unix.sendto: invalid file-descr";
  unsafe_sendto file_descr off len str flags sockaddr

let rec accept ?cloexec ({ fd; non_blocking; _ } as file_descr) =
  if is_dgram file_descr then invalid_arg "Miou_unix.accept: invalid file-descr";
  if non_blocking then (
    match Unix.accept ?cloexec fd with
    | exception Unix.(Unix_error (EINTR, _, _)) -> accept ?cloexec file_descr
    | exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
        blocking_read ~name:"accept" fd;
        accept ?cloexec file_descr
    | fd, sockaddr ->
        Unix.set_nonblock fd;
        let file_descr = { fd; non_blocking= true; kind= Some STREAM } in
        (file_descr, sockaddr))
  else
    let rec go () =
      match Unix.accept ?cloexec fd with
      | exception Unix.(Unix_error (EINTR, _, _)) -> go ()
      | fd, sockaddr ->
          Unix.set_nonblock fd;
          let file_descr = { fd; non_blocking= true; kind= Some STREAM } in
          (file_descr, sockaddr)
    in
    blocking_read fd; go ()

let rec connect ({ fd; non_blocking; _ } as file_descr) sockaddr =
  if not non_blocking then
    invalid_arg
      "Miou_unix.connect: we expect a file descriptor in the non-blocking mode";
  if is_dgram file_descr then
    invalid_arg "Miou_unix.connect: invalid file-descr";
  match Unix.connect fd sockaddr with
  | () -> ()
  | exception Unix.(Unix_error (EINTR, _, _)) -> connect file_descr sockaddr
  | exception Unix.(Unix_error (EINPROGRESS, _, _)) -> (
      blocking_write ~name:"connect" fd;
      match Unix.getsockopt_error fd with
      | None -> ()
      | Some err -> raise (Unix.Unix_error (err, "connect", "")))

let close { fd; _ } = Unix.close fd

let sleep until =
  let syscall = Miou.syscall () in
  let domain = domain () in
  let elt =
    { time= Unix.gettimeofday () +. until; syscall; cancelled= false }
  in
  let fn () = Heapq.insert elt domain.sleepers in
  Miou.suspend ~fn syscall

module Ownership = struct
  type old = file_descr

  type file_descr = {
      fd: Unix.file_descr
    ; non_blocking: bool
    ; resource: Miou.Ownership.t
    ; kind: kind option
  }

  let bind_and_listen ?backlog { fd; non_blocking; resource; kind } sockaddr =
    Miou.Ownership.check resource;
    bind_and_listen ?backlog { fd; non_blocking; kind } sockaddr

  let read { fd; non_blocking; resource; kind } ?off ?len buf =
    Miou.Ownership.check resource;
    read { fd; non_blocking; kind } ?off ?len buf

  let really_read { fd; non_blocking; resource; kind } ?off ?len buf =
    Miou.Ownership.check resource;
    really_read { fd; non_blocking; kind } ?off ?len buf

  let write { fd; non_blocking; resource; kind } ?off ?len str =
    Miou.Ownership.check resource;
    write { fd; non_blocking; kind } ?off ?len str

  let accept ?cloexec { fd; non_blocking; resource; kind } =
    Miou.Ownership.check resource;
    let ({ fd; non_blocking; kind } : old), sockaddr =
      accept ?cloexec { fd; non_blocking; kind }
    in
    let resource = Miou.Ownership.create ~finally:Unix.close fd in
    Miou.Ownership.own resource;
    ({ fd; non_blocking; resource; kind }, sockaddr)

  let connect { fd; non_blocking; resource; kind } sockaddr =
    Miou.Ownership.check resource;
    connect { fd; non_blocking; kind } sockaddr

  let close { fd; resource; _ } =
    Miou.Ownership.disown resource;
    Unix.close fd

  let of_file_descr ?(non_blocking = true) fd =
    let resource = Miou.Ownership.create ~finally:Unix.close fd in
    if non_blocking then Unix.set_nonblock fd else Unix.clear_nonblock fd;
    Miou.Ownership.own resource;
    match Unix.getsockopt_int fd Unix.SO_TYPE with
    | 1 -> { fd; non_blocking; resource; kind= Some STREAM }
    | 2 -> { fd; non_blocking; resource; kind= Some DGRAM }
    | _ ->
        invalid_arg
          "Miou_unix.Ownership.of_file_descr: invalid file-descriptor (neither \
           TCP nor UDP)"
    | exception _ -> { fd; non_blocking; resource; kind= None }

  let to_file_descr { fd; _ } = fd
  let resource { resource; _ } = resource

  let tcpv4 () =
    let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let resource = Miou.Ownership.create ~finally:Unix.close fd in
    Unix.set_nonblock fd;
    Miou.Ownership.own resource;
    { fd; non_blocking= true; resource; kind= Some STREAM }

  let tcpv6 () =
    let fd = Unix.socket Unix.PF_INET6 Unix.SOCK_STREAM 0 in
    let resource = Miou.Ownership.create ~finally:Unix.close fd in
    Unix.set_nonblock fd;
    Miou.Ownership.own resource;
    { fd; non_blocking= true; resource; kind= Some STREAM }

  let udpv4 () =
    let fd = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
    let resource = Miou.Ownership.create ~finally:Unix.close fd in
    Unix.set_nonblock fd;
    Miou.Ownership.own resource;
    { fd; non_blocking= true; resource; kind= Some DGRAM }

  let udpv6 () =
    let fd = Unix.socket Unix.PF_INET6 Unix.SOCK_DGRAM 0 in
    let resource = Miou.Ownership.create ~finally:Unix.close fd in
    Unix.set_nonblock fd;
    Miou.Ownership.own resource;
    { fd; non_blocking= true; resource; kind= Some DGRAM }

  let recvfrom { fd; non_blocking; resource; kind } ?off ?len buf flags =
    Miou.Ownership.check resource;
    recvfrom { fd; non_blocking; kind } ?off ?len buf flags

  let sendto { fd; non_blocking; resource; kind } ?off ?len str flags sockaddr =
    Miou.Ownership.check resource;
    sendto { fd; non_blocking; kind } ?off ?len str flags sockaddr
end

let rec sleeper domain =
  match Heapq.find_min_exn domain.sleepers with
  | exception Heapq.Empty -> None
  | { cancelled= true; _ } ->
      Heapq.delete_min_exn domain.sleepers;
      sleeper domain
  | { time; _ } -> Some time

let in_the_past t = t = 0. || t <= Unix.gettimeofday ()

let rec collect domain signals =
  match Heapq.find_min_exn domain.sleepers with
  | exception Heapq.Empty -> signals
  | { cancelled= true; _ } ->
      Heapq.delete_min_exn domain.sleepers;
      collect domain signals
  | { time; syscall; _ } when in_the_past time ->
      Heapq.delete_min_exn domain.sleepers;
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
  match Unix.read fd buf 0 (Bytes.length buf) with
  | _ -> ()
  | exception Unix.(Unix_error (EAGAIN, _, _)) -> ()

let select uid interrupt ~block cancelled_syscalls =
  let domain = domain () in
  clean domain cancelled_syscalls;
  let rds = file_descrs domain.readers in
  let wrs = file_descrs domain.writers in
  let timeout =
    match (sleeper domain, block) with
    | None, true -> -1.0
    | (None | Some _), false -> 0.0
    | Some value, true ->
        let value = value -. Unix.gettimeofday () in
        Float.max value 0.0
  in
  Logs.debug (fun m ->
      m "[%a] [readers:%dw, writers:%dw, sleepers:%dw, revert:%dw]"
        Miou.Domain.Uid.pp uid
        Obj.(reachable_words (repr domain.readers))
        Obj.(reachable_words (repr domain.writers))
        Obj.(reachable_words (repr domain.sleepers))
        Obj.(reachable_words (repr domain.revert)));
  Logs.debug (fun m -> m "select(%f)" timeout);
  match Unix.select (interrupt :: rds) wrs [] timeout with
  | exception Unix.(Unix_error (EINTR, _, _)) ->
      Logs.debug (fun m ->
          m "[%a] interrupted by the system" Miou.Domain.Uid.pp uid);
      collect domain []
  | exception Unix.(Unix_error (err, f, arg)) ->
      Logs.err (fun m ->
          m "[%a] exception %s(%s): %s" Miou.Domain.Uid.pp uid f arg
            (Unix.error_message err));
      collect domain []
  | [], [], _ -> collect domain []
  | rds, wrs, _ ->
      if interrupted interrupt rds then begin
        Logs.debug (fun m ->
            m "[%a] interrupted by Miou" Miou.Domain.Uid.pp uid);
        intr interrupt
      end;
      let signals = collect domain [] in
      let signals = transmit_fds signals domain.revert domain.readers rds in
      let signals = transmit_fds signals domain.revert domain.writers wrs in
      signals

let signal = Bytes.make 1 '\000'

let interrupt oc () =
  match Unix.single_write oc signal 0 1 with
  | n -> assert (n = 1) (* XXX(dinosaure): paranoid mode. *)
  | exception Unix.(Unix_error (EAGAIN, _, _)) -> ()

let is_windows = Sys.os_type = "Win32"

let events domain =
  let ic, oc = Unix.pipe ~cloexec:true () in
  if not is_windows then Unix.set_nonblock ic;
  if not is_windows then Unix.set_nonblock oc;
  let select = select domain ic in
  let finaliser () = Unix.close ic; Unix.close oc in
  { Miou.interrupt= interrupt oc; select; finaliser }

let run ?g ?domains fn = Miou.run ~events ?g ?domains fn
