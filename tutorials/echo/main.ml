let handler fd =
  let buf = Bytes.create 0x100 in
  let rec go () =
    let len = Miou_unix.read fd buf ~off:0 ~len:(Bytes.length buf) in
    if len > 0 then begin
      Miou_unix.write fd buf ~off:0 ~len;
      go ()
    end
    else Miou_unix.close fd
  in
  go

let listen sockaddr =
  let fd = Miou_unix.tcpv4 () in
  Miou_unix.bind_and_listen fd sockaddr;
  fd

let rec clean_up orphans =
  match Miou.care orphans with
  | Some prm -> Miou.await_exn prm; clean_up orphans
  | None -> ()

let server sockaddr =
  let rec server orphans fd =
    clean_up orphans;
    let fd', _sockaddr = Miou_unix.accept fd in
    let _ = Miou.call ~orphans ~give:[ Miou_unix.owner fd' ] (handler fd') in
    server orphans fd
  in
  let orphans = Miou.orphans () in
  server orphans (listen sockaddr)

let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 9000)

let () =
  Miou_unix.run @@ fun () ->
  Miou.parallel server (List.init (Miou.Domain.count ()) (Fun.const addr))
  |> List.iter (function Ok () -> () | Error exn -> raise exn)
