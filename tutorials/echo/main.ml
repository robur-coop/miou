let anchor = Unix.gettimeofday ()

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("[%a]%a[%a][%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Cyan (fmt "%.04f"))
        (Unix.gettimeofday () -. anchor)
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Blue int)
        (Stdlib.Domain.self () :> int)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt
  in
  { Logs.report }

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stderr)
let () = Logs.set_level ~all:true (Some Logs.Debug)
let () = Logs_threaded.enable ()
let () = Printexc.record_backtrace true

let rec go fd buf =
  let len = Miou_unix.read fd buf ~off:0 ~len:(Bytes.length buf) in
  Logs.debug (fun m ->
      m "got %d byte(s) from %d" len (Obj.magic (Miou_unix.to_file_descr fd)));
  if len > 0 then begin
    Miou_unix.write fd (Bytes.unsafe_to_string buf) ~off:0 ~len;
    go fd buf
  end

let pp_sockaddr ppf = function
  | Unix.ADDR_UNIX str -> Fmt.pf ppf "<%s>" str
  | Unix.ADDR_INET (inet_addr, port) ->
      Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr inet_addr) port

let pp_prm ppf (uid, runner, resources) =
  Fmt.pf ppf "[%a:%a](%d)" Miou.Domain.Uid.pp runner Miou.Promise.Uid.pp uid
    resources

let handler fd () =
  let buf = Bytes.create 0x100 in
  let file_descr = Miou_unix.to_file_descr fd in
  let sockaddr = Unix.getpeername file_descr in
  Logs.debug (fun m ->
      m "handle %d (%a)" (Obj.magic file_descr) pp_sockaddr sockaddr);
  go fd buf;
  Logs.debug (fun m ->
      m "close file-descr %d (%a)" (Obj.magic file_descr) pp_sockaddr sockaddr);
  Miou_unix.close fd

let listen sockaddr =
  let fd = Miou_unix.tcpv4 () in
  Miou_unix.bind_and_listen fd sockaddr;
  fd

let rec clean_up orphans =
  match Miou.care orphans with
  | Some (Some prm) -> Miou.await_exn prm; clean_up orphans
  | Some None | None -> ()

exception Timeout

let with_timeout ~ts ?(give = []) fn =
  let timeout () = Miou_unix.sleep ts; raise Timeout in
  Miou.await_first [ Miou.call_cc timeout; Miou.call_cc ~give fn ]

let rec accept ~orphans fd =
  match
    with_timeout ~ts:1. ~give:[ Miou_unix.owner fd ] @@ fun () ->
    let fd', _ = Miou_unix.accept fd in
    Miou_unix.disown fd; Miou_unix.transfer fd'
  with
  | Error Timeout -> clean_up orphans; accept ~orphans fd
  | Error exn -> raise exn
  | Ok fd' ->
      let sockaddr = Unix.getpeername (Miou_unix.to_file_descr fd') in
      (fd', sockaddr)

let server sockaddr =
  let prm = Miou.self () in
  let rec server orphans fd =
    let words = Miou.stats () in
    Logs.debug (fun m -> m "%a: %dw" pp_prm prm words);
    clean_up orphans;
    let fd', _sockaddr = Miou_unix.accept fd in
    let _ = Miou.call ~orphans ~give:[ Miou_unix.owner fd' ] (handler fd') in
    Miou.Ownership.disown (Miou_unix.owner fd');
    server orphans fd
  in
  let orphans = Miou.orphans () in
  server orphans (listen sockaddr)

let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 9000)

let () =
  Miou_unix.run @@ fun () ->
  let prm = Miou.call_cc @@ fun () -> server addr in
  Miou.parallel server (List.init (Miou.Domain.count ()) (Fun.const addr))
  |> List.iter (function Ok () -> () | Error exn -> raise exn);
  Miou.await_exn prm
