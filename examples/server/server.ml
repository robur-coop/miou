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

exception Stop

let wait ~stop () = Miou_unix.Cond.wait stop; raise Stop

let accept_or_stop ?stop file_descr =
  match stop with
  | None -> Some (Miou_unix.accept file_descr)
  | Some stop -> begin
      let accept =
        Miou.call_cc ~give:[ Miou_unix.owner file_descr ] @@ fun () ->
        let file_descr', sockaddr = Miou_unix.accept file_descr in
        Miou_unix.disown file_descr;
        (Miou_unix.transfer file_descr', sockaddr)
      in
      let wait = Miou.call_cc (wait ~stop) in
      Miou.await_first [ accept; wait ] |> function
      | Ok value -> Some value
      | Error Stop -> None
      | Error exn -> raise exn
    end

let rec terminate orphans =
  match Miou.care orphans with
  | Some (Some prm) -> Miou.await_exn prm; terminate orphans
  | Some None -> Miou.yield (); terminate orphans
  | None -> ()

let rec clean orphans =
  match Miou.care orphans with
  | Some (Some prm) -> Miou.await_exn prm; clean orphans
  | Some None | None -> ()

let listen sockaddr =
  let file_descr =
    match sockaddr with
    | Unix.ADDR_INET (inet_addr, _) ->
        if Unix.is_inet6_addr inet_addr then Miou_unix.tcpv6 ()
        else Miou_unix.tcpv4 ()
    | _ -> failwith "Invalid address"
  in
  Miou_unix.bind_and_listen file_descr sockaddr;
  file_descr

let server ?stop ~handler sockaddr =
  let rec go orphans file_descr =
    match accept_or_stop ?stop file_descr with
    | None -> terminate orphans
    | Some (file_descr', sockaddr') ->
        clean orphans;
        let give = [ Miou_unix.owner file_descr' ] in
        let _ =
          Miou.call ~orphans ~give @@ fun () -> handler file_descr' sockaddr'
        in
        Miou_unix.disown file_descr';
        go orphans file_descr
  in
  let file_descr = listen sockaddr in
  go (Miou.orphans ()) file_descr;
  Miou_unix.disown file_descr

let pp_sockaddr ppf = function
  | Unix.ADDR_INET (inet_addr, port) ->
      Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr inet_addr) port
  | Unix.ADDR_UNIX name -> Fmt.pf ppf "<%s>" name

let handler file_descr sockaddr =
  Logs.debug (fun m -> m "Handle %a" pp_sockaddr sockaddr);
  Miou_unix.close file_descr;
  Logs.debug (fun m -> m "Close %a" pp_sockaddr sockaddr)

let stop = Miou_unix.Cond.make ()

let do_stop _ =
  Logs.debug (fun m -> m "Stop the server");
  Miou_unix.Cond.broadcast stop

let () =
  Miou_unix.run @@ fun () ->
  let () = Sys.set_signal Sys.sigint (Signal_handle do_stop) in
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 3333) in
  let prm = Miou.call_cc @@ fun () -> server ~stop ~handler addr in
  Miou.parallel (server ~stop ~handler)
    (List.init (Miou.Domain.count ()) (Fun.const addr))
  |> List.iter (function Ok () -> () | Error exn -> raise exn);
  Miou.await_exn prm
