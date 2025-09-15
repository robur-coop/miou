let spawn tmp prgm args =
  let env = Unix.environment () in
  let runtime_events =
    [|
       "OCAML_RUNTIME_EVENTS_START=1"; "OCAML_RUNTIME_EVENTS_DIR=" ^ tmp
     ; "OCAML_RUNTIME_PRESERVE=1"
    |]
  in
  let env = Array.append runtime_events env in
  let null =
    Unix.openfile Filename.null [ Unix.O_WRONLY; Unix.O_KEEPEXEC ] 0o666
  in
  let pid = Unix.create_process_env prgm args env Unix.stdin null null in
  let finally () =
    Unix.close null;
    Unix.kill pid Sys.sigkill;
    let runtime_events_filename =
      Filename.concat tmp (string_of_int pid ^ ".events")
    in
    Unix.unlink runtime_events_filename
  in
  (pid, finally)

let run _ tmp filename prgm args =
  Miou_unix.run ~domains:2 @@ fun () ->
  let filename = Option.map Fpath.to_string filename in
  let tmp = Fpath.to_string tmp in
  let args = Array.of_list args in
  let pid, finally = spawn tmp prgm args in
  Logs.debug (fun m ->
      m "`%s %a` spawned" prgm Fmt.(array ~sep:(any "@ ") string) args);
  Fun.protect ~finally @@ fun () ->
  try Miou_trace.run ~tmp ?filename pid
  with exn ->
    Logs.err (fun m -> m "Unexpected exception: %s" (Printexc.to_string exn))

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

open Cmdliner

let output_options = "OUTPUT OPTIONS"

let verbosity =
  let env = Cmd.Env.info "MIOU_RECORD_LOGS" in
  Logs_cli.level ~docs:output_options ~env ()

let renderer =
  let env = Cmd.Env.info "MIOU_RECORD_FMT" in
  Fmt_cli.style_renderer ~docs:output_options ~env ()

let utf_8 =
  let doc = "Allow binaries to emit UTF-8 characters." in
  let env = Cmd.Env.info "MIOU_RECORD_UTF_8" in
  Arg.(value & opt bool true & info [ "with-utf-8" ] ~doc ~env)

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let with_metadata header _tags k ppf fmt =
      Fmt.kpf k ppf
        ("[%a]%a[%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Cyan int)
        (Stdlib.Domain.self () :> int)
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt
  in
  { Logs.report }

let setup_logs utf_8 style_renderer level =
  Fmt_tty.setup_std_outputs ~utf_8 ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (reporter Fmt.stderr);
  Option.is_none level

let setup_logs = Term.(const setup_logs $ utf_8 $ renderer $ verbosity)

let tmp =
  let default = Fpath.v (Filename.get_temp_dir_name ()) in
  let default = Fpath.to_dir_path default in
  let env = Cmd.Env.info "MIOU_TEMP" in
  let doc = "The temporary directory." in
  let parser str =
    match Fpath.of_string str with
    | Ok v when Sys.file_exists str && Sys.is_directory str ->
        Ok (Fpath.to_dir_path v)
    | _ -> error_msgf "Invalid temporary directory: %S" str
  in
  let existing_directory = Arg.conv (parser, Fpath.pp) in
  let open Arg in
  value
  & opt existing_directory default
  & info [ "t"; "tmp" ] ~env ~doc ~docv:"DIRECTORY"

let filename =
  let doc = "The Fushia trace filename." in
  let parser str =
    match Fpath.of_string str with
    | Ok v when Fpath.is_file_path v && Sys.file_exists str = false -> Ok v
    | Ok v -> error_msgf "%a already exists" Fpath.pp v
    | Error _ as err -> err
  in
  let non_existing_filename = Arg.conv (parser, Fpath.pp) in
  let open Arg in
  value
  & opt (some non_existing_filename) None
  & info [ "o"; "output" ] ~doc ~docv:"FILENAME"

let prgm =
  let doc = "Invoke program $(docv)." in
  let open Arg in
  required & pos 0 (some string) None & info [] ~doc ~docv:"TOOL"

let args =
  let doc =
    "Argument for the program. Start with a $(b,--) token otherwise options \
     get interpreted by $(mname)."
  in
  let open Arg in
  value & pos_right 0 string [] & info [] ~doc ~docv:"ARG"

let cmd =
  let doc = "Record events from a Miou program." in
  let man = [] in
  let open Term in
  let info = Cmd.info "record" ~doc ~man in
  let term = const run $ setup_logs $ tmp $ filename $ prgm $ args in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
