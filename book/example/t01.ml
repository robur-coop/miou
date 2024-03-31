let rec echo client =
  let buf = Bytes.create 0x100 in
  let len = Miou_unix.read client buf 0 (Bytes.length buf) in
  if len = 0 then Miou_unix.close client
  else
    let str = Bytes.sub_string buf 0 len in
    let _ = Miou_unix.write client str 0 len in echo client

let rec clean_up orphans = match Miou.care orphans with
  | None | Some None -> ()
  | Some (Some prm) -> match Miou.await prm with
    | Ok () -> clean_up orphans
    | Error exn -> raise exn

let server () =
  let socket = Miou_unix.tcpv4 () in
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, 4000) in
  Miou_unix.bind_and_listen socket sockaddr;
  let orphans = Miou.orphans () in
  while true do
    clean_up orphans;
    let client, _ = Miou_unix.accept socket in
    ignore (Miou.call_cc ~orphans (fun () -> echo client))
  done;
  Miou_unix.close socket

let () = Miou_unix.run @@ fun () ->
  let domains = Stdlib.Domain.recommended_domain_count () - 1 in
  let domains = List.init domains (Fun.const ()) in
  let prm = Miou.call_cc server in
  Miou.await prm :: Miou.parallel server domains
  |> List.iter @@ function
  | Ok () -> ()
  | Error exn -> raise exn
