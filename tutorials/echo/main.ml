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
    match Miou_unix.Ownership.read fd buf 0 (Bytes.length buf) with
    | 0 -> Miou_unix.Ownership.close fd
    | len ->
        let str = Bytes.unsafe_to_string buf in
        Miou_unix.Ownership.write fd str 0 len;
        go buf
  in
  go (Bytes.create 0x100)

let rec clean_up orphans =
  match Miou.care orphans with
  | None | Some None -> ()
  | Some (Some prm) -> Miou.await_exn prm; clean_up orphans

let server sockaddr =
  let rec go orphans fd =
    clean_up orphans;
    let fd', _sockaddr = Miou_unix.Ownership.accept fd in
    let _ =
      Miou.call
        ~give:[ Miou_unix.Ownership.resource fd' ]
        ~orphans (handler fd')
    in
    go orphans fd
  in
  go (Miou.orphans ()) (listen sockaddr)

let localhost_3000 = Unix.ADDR_INET (Unix.inet_addr_any, 3000)

let () =
  Miou_unix.run @@ fun () ->
  let servers = List.init 3 (Fun.const localhost_3000) in
  let prm = Miou.call_cc @@ fun () -> server localhost_3000 in
  ignore (Miou.parallel server servers);
  Miou.await_exn prm
