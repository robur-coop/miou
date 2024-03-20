let anchor = Mtime_clock.now ()

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let with_metadata header _tags k ppf fmt =
      let timestamp = Mtime.span (Mtime_clock.now ()) anchor in
      Format.kfprintf k ppf
        ("[+%a]%a[%a][%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Magenta Mtime.Span.pp)
        timestamp Logs_fmt.pp_header (level, header)
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
let () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)

let getaddrinfo dns =
  { Happy.getaddrinfo= (fun record host -> Mdns.getaddrinfo dns record host) }

let () =
  Miou_unix.run @@ fun () ->
  let daemon, resolver = Happy.stack () in
  let dns = Mdns.create resolver in
  Happy.inject_resolver ~getaddrinfo:(getaddrinfo dns) resolver;
  let f _resp buf str = Buffer.add_string buf str; buf in
  match
    Httpcats.request ~resolver ~f ~uri:Sys.argv.(1) (Buffer.create 0x100)
  with
  | Ok (_, body) ->
      Happy.kill daemon;
      Format.printf "@[<hov>%a@]\n%!"
        (Hxd_string.pp Hxd.default)
        (Buffer.contents body)
  | Error err ->
      Happy.kill daemon;
      Format.eprintf "%a\n%!" Httpcats.pp_error err
