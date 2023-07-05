let osau_re = Domain_name.of_string_exn "osau.re" |> Domain_name.host_exn
let robur_coop = Domain_name.of_string_exn "robur.coop" |> Domain_name.host_exn
let () = Printexc.record_backtrace true
let () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("[%a]%a[%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Blue Miou.Did.pp)
        (Miou.Did.self ()) Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt
  in
  { Logs.report }

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stdout)
let () = Logs.set_level ~all:true (Some Logs.Debug)
let () = Logs_threaded.enable ()

let print = function
  | Ok ipaddr -> Format.printf "%a\n%!" Ipaddr.V4.pp ipaddr
  | Error (`Msg msg) -> Format.eprintf "Got an error: %s\n%!" msg

let or_raise = function Ok v -> v | Error exn -> raise exn

let () =
  Miouu.run @@ fun () ->
  let dns =
    Mdns.connect
      ~nameservers:[ "tcp:8.8.8.8"; "tcp:127.0.0.1"; "tcp:1.1.1.1" ]
      ()
  in
  let domains =
    [ Mdns.gethostbyname dns osau_re; Mdns.gethostbyname dns robur_coop ]
  in
  List.iter print domains;
  or_raise (Mdns.kill dns)
