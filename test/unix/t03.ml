open Miouu

let listen sockaddr =
  let socket =
    Unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0
  in
  Unix.setsockopt socket Unix.SO_REUSEADDR true;
  Unix.setsockopt socket Unix.SO_REUSEPORT true;
  Unix.bind socket sockaddr;
  Unix.listen socket 64;
  of_file_descr socket

let sockaddr_to_string = function
  | Unix.ADDR_UNIX str -> str
  | Unix.ADDR_INET (inet_addr, port) ->
      Unix.string_of_inet_addr inet_addr ^ ":" ^ string_of_int port

let rec cat (fd, buf) =
  let len = Miouu.read fd buf ~off:0 ~len:0x100 in
  if len > 0 then begin
    Miouu.write fd (Bytes.unsafe_to_string buf) ~off:0 ~len;
    cat (fd, buf)
  end
  else Unix.close (to_file_descr fd)

let prgm sockaddr =
  let rec server fd =
    let _, fd', _ = Miouu.accept fd in
    cat (fd', Bytes.create 0x100);
    server fd
  in
  fun () -> server (listen sockaddr)

let () = Miouu.run (prgm (Unix.ADDR_INET (Unix.inet_addr_loopback, 9000)))
