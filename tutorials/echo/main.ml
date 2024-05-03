module Logs = Miou.Logs.Make (struct
  let src = "echo"
end)

let listen = function
  | Unix.ADDR_INET (inet_addr, _) as sockaddr ->
      if Unix.is_inet6_addr inet_addr then (
        let tcpv6 = Miou_unix.Ownership.tcpv6 () in
        Miou_unix.Ownership.bind_and_listen tcpv6 sockaddr;
        tcpv6)
      else
        let tcpv4 = Miou_unix.Ownership.tcpv4 () in
        Miou_unix.Ownership.bind_and_listen tcpv4 sockaddr;
        tcpv4
  | _ -> failwith "Impossible to listen into a unix file"

let handler fd () =
  let rec go buf =
    match Miou_unix.Ownership.read fd buf ~off:0 ~len:(Bytes.length buf) with
    | 0 -> Miou_unix.Ownership.close fd
    | len ->
        let str = Bytes.unsafe_to_string buf in
        Miou_unix.Ownership.write fd str ~off:0 ~len;
        go buf
  in
  go (Bytes.create 0x100)

let stop (m, c, v) =
  Miou.Mutex.protect m @@ fun () ->
  while !v = false do
    Miou.Condition.wait c m
  done

exception Stop

let accept_or_stop v fd =
  let accept () =
    let fd', sockaddr = Miou_unix.Ownership.accept fd in
    Miou.Ownership.transfer (Miou_unix.Ownership.resource fd);
    Miou.Ownership.transfer (Miou_unix.Ownership.resource fd');
    (fd', sockaddr)
  in
  match
    Miou.await_first
      [
        Miou.call_cc (fun () -> stop v; raise Stop)
      ; Miou.call_cc ~give:[ Miou_unix.Ownership.resource fd ] accept
      ]
  with
  | Error Stop -> `Stop
  | Ok (fd, addr) -> `Accept (fd, addr)
  | Error exn -> raise exn

let rec clean_up orphans =
  match Miou.care orphans with
  | None | Some None -> ()
  | Some (Some prm) -> Miou.await_exn prm; clean_up orphans

let rec terminate orphans =
  match Miou.care orphans with
  | None -> ()
  | Some None -> Miou.yield ()
  | Some (Some prm) -> Miou.await_exn prm; terminate orphans

let server (stop, sockaddr) =
  let rec go orphans fd =
    clean_up orphans;
    match accept_or_stop stop fd with
    | `Stop -> terminate orphans
    | `Accept (fd', _) ->
        let _ =
          Miou.call
            ~give:[ Miou_unix.Ownership.resource fd' ]
            ~orphans (handler fd')
        in
        go orphans fd
  in
  go (Miou.orphans ()) (listen sockaddr)

let localhost_3000 = Unix.ADDR_INET (Unix.inet_addr_any, 3000)

type stop = Miou.Mutex.t * Miou.Condition.t * bool ref

let stop (m, c, v) _signal =
  Miou.Mutex.protect m @@ fun () ->
  v := true;
  Miou.Condition.broadcast c

let () =
  Miou_unix.run @@ fun () ->
  let v = (Miou.Mutex.create (), Miou.Condition.create (), ref false) in
  ignore (Miou.sys_signal Sys.sigint (Sys.Signal_handle (stop v)));
  let servers = List.init 3 (Fun.const (v, localhost_3000)) in
  let prm = Miou.call_cc @@ fun () -> server (v, localhost_3000) in
  ignore (Miou.parallel server servers);
  Miou.await_exn prm
