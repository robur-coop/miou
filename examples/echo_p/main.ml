let listen sockaddr =
  let fd = Miouu.tcpv4 () in
  Miouu.bind_and_listen fd sockaddr;
  fd

let sockaddr_to_string = function
  | Unix.ADDR_UNIX str -> str
  | Unix.ADDR_INET (inet_addr, port) ->
      Unix.string_of_inet_addr inet_addr ^ ":" ^ string_of_int port

let handler fd =
  let buf = Bytes.create 0x100 in
  let rec go () =
    let len = Miouu.read fd buf ~off:0 ~len:(Bytes.length buf) in
    if len > 0 then (
      Miouu.write fd (Bytes.unsafe_to_string buf) ~off:0 ~len;
      go ())
    else Miouu.close fd
  in
  go

let rec clean orphans =
  match Miou.care orphans with
  | None -> ()
  | Some prm -> Miou.await_exn prm; clean orphans

let prgm sockaddr =
  let rec server orphans fd =
    clean orphans;
    let fd', sockaddr = Miouu.accept fd in
    Format.printf "- new connection from %s\n%!" (sockaddr_to_string sockaddr);
    ignore (Miou.call ~give:[ Miouu.owner fd' ] ~orphans (handler fd'));
    server orphans fd
  in
  fun () -> server (Miou.orphans ()) (listen sockaddr)

let () = Miouu.run (prgm (Unix.ADDR_INET (Unix.inet_addr_loopback, 9000)))
