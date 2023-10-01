let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a][%a]: " ^^ fmt ^^ "\n%!")
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

(* let () = Logs.set_level ~all:true (Some Logs.Debug) *)
let () = Logs_threaded.enable ()
let () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)
let out = Mutex.create ()

let pr fmt =
  let finally () = Mutex.unlock out in
  Mutex.lock out;
  Fun.protect ~finally @@ fun () -> Format.printf fmt

let epr fmt =
  let finally () = Mutex.unlock out in
  Mutex.lock out;
  Fun.protect ~finally @@ fun () -> Format.eprintf fmt

let pp_msg ppf (`Msg msg) = Fmt.string ppf msg
let osau_re = Domain_name.(host_exn (of_string_exn "osau.re"))
let google_com = Domain_name.(host_exn (of_string_exn "google.com"))

let getaddrinfo dns =
  { Happy.getaddrinfo= (fun record host -> Mdns.getaddrinfo dns record host) }

let () =
  Miou_unix.run @@ fun () ->
  let daemon, stack = Happy.stack () in
  let dns = Mdns.create stack in
  Happy.inject_resolver ~getaddrinfo:(getaddrinfo dns) stack;
  for i = 0 to 10_000 do
    match Happy.connect_endpoint stack "google.com" [ 443 ] with
    | Ok (_, fd) -> Miou_unix.close fd
    | Error (`Msg msg) -> failwith msg
  done;
  Happy.kill daemon
