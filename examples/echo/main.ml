let listen sockaddr =
  let fd = Miou_unix.tcpv4 () in
  Miou_unix.bind_and_listen fd sockaddr;
  fd

let sockaddr_to_string = function
  | Unix.ADDR_UNIX str -> str
  | Unix.ADDR_INET (inet_addr, port) ->
      Unix.string_of_inet_addr inet_addr ^ ":" ^ string_of_int port

let handler fd =
  let buf = Bytes.create 0x100 in
  let rec go () =
    let len = Miou_unix.read fd buf ~off:0 ~len:(Bytes.length buf) in
    if len > 0 then (
      Miou_unix.write fd (Bytes.unsafe_to_string buf) ~off:0 ~len;
      go ())
    else Miou_unix.close fd
  in
  go

let rec clean orphans =
  match Miou.care orphans with
  | None -> ()
  | Some prm -> Miou.await_exn prm; clean orphans

let prgm sockaddr () =
  let rec server orphans fd =
    clean orphans;
    let fd', _sockaddr = Miou_unix.accept fd in
    ignore (Miou.call ~give:[ Miou_unix.owner fd' ] ~orphans (handler fd'));
    server orphans fd
  in
  let serve () = server (Miou.orphans ()) (listen sockaddr) in
  Miou.parallel serve [ (); (); (); () ]
  |> List.iter (function Error exn -> raise exn | Ok () -> ())

let () = Miou_unix.run (prgm (Unix.ADDR_INET (Unix.inet_addr_loopback, 9000)))
