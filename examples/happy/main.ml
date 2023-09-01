let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (Logs_fmt.reporter ())
let () = Logs.set_level ~all:true (Some Logs.Debug)
let () = Logs_threaded.enable ()
let out = Mutex.create ()

let pr fmt =
  let finally () = Mutex.unlock out in
  Mutex.lock out;
  Fun.protect ~finally @@ fun () -> Format.printf fmt

let epr fmt =
  let finally () = Mutex.unlock out in
  Mutex.lock out;
  Fun.protect ~finally @@ fun () -> Format.eprintf fmt

let nss0 =
  [
    `Plaintext (Ipaddr.of_string_exn "127.0.0.1", 53)
  ; `Plaintext (Ipaddr.of_string_exn "1.1.1.1", 53)
  ; `Plaintext (Ipaddr.of_string_exn "8.8.8.8", 53)
  ]

let nss1 =
  [
    `Plaintext (Ipaddr.of_string_exn "8.8.8.8", 53)
  ; `Plaintext (Ipaddr.of_string_exn "1.1.1.1", 53)
  ]

let program t nameservers () =
  let addr, fd = Happy.connect_to_nameservers t nameservers in
  pr "Connected to %a\n%!" Happy.pp_sockaddr addr;
  Miou_unix.close fd

let () =
  Miou_unix.run @@ fun () ->
  let daemon, t = Happy.create ~timeout:(Duration.of_sec 1) in
  let prm0 = Miou.call_cc (program t nss0) in
  let prm1 = Miou.call_cc (program t nss1) in
  Miou.await_all [ prm0; prm1 ]
  |> List.iter (function
       | Ok () -> ()
       | Error exn ->
           epr "Connection failed with: %S\n%!" (Printexc.to_string exn));
  Miou.cancel daemon
