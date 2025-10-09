open B0_kit.V000

[@@@ocamlformat "disable"]

let ( ~/ ) x = Fpath.v x
let unix = B0_ocaml.libname "unix"
let miou = B0_ocaml.libname "miou"
let miou_unix = B0_ocaml.libname "miou.unix"
let miou_backoff = B0_ocaml.libname "miou.backoff"
let miou_sync = B0_ocaml.libname "miou.sync"
let dune_configurator = B0_ocaml.libname "dune-configurator"
let std = B0_ocaml.libname "std"

let dune_discover =
  let doc = "miou_poll.h and miou_poll_config.ml generator (with dune)" in
  let srcs = [ `File ~/"lib/conf/discover.ml" ] in
  let requires = [ dune_configurator ] in
  B0_ocaml.exe "discover" ~doc ~srcs ~requires

let std_lib =
  let doc = "An extended standard library (for internal tools)" in
  let srcs = [ `File ~/"lib/conf/std.ml" ] in
  B0_ocaml.lib std ~doc ~srcs

let cc_exe =
  let doc = "C compiler with OCaml C flags" in
  let srcs = [ `File ~/"lib/conf/cc.ml" ] in
  let requires = [ std ] in
  B0_ocaml.exe "cc" ~doc ~srcs ~requires

let has_ppoll_exe =
  let doc = "miou_poll.h generator (with b0)" in
  let srcs = [ `File ~/"lib/conf/has_ppoll.ml" ] in
  let requires = [ std ] in
  B0_ocaml.exe "has_ppoll" ~doc ~srcs ~requires

let miou_poll_header =
  let doc = "Generation of miou_poll.h" in
  B0_unit.of_action ~doc ~units:[has_ppoll_exe] "miou_poll_header" @@ fun env _ ~args:_ ->
  let open Result.Syntax in
  let output = B0_env.in_scope_dir env ~/"lib/miou_poll.h" in
  let has_ppoll_c = B0_env.in_scope_dir env ~/"lib/conf/has_ppoll.c" in
  let* has_ppoll_exe = B0_env.unit_exe_file_cmd env has_ppoll_exe in
  let stdout = Os.Cmd.out_file ~force:true ~make_path:false output in
  Os.Cmd.run Cmd.(has_ppoll_exe %% path has_ppoll_c) ~stdout

let generate_exe =
  let p, set = Fut.make () in
  let meta =
    B0_meta.empty
    |> B0_meta.tag B0_meta.exe
    |> B0_meta.add B0_unit.exe_file p in
  let doc = "miou_poll_config.ml generator (with b0)" in
  B0_unit.of_action ~doc ~meta ~units:[cc_exe; miou_poll_header] "generate" @@ fun env _ ~args:_ ->
  let open Result.Syntax in
  let* cc_exe = B0_env.unit_exe_file_cmd env cc_exe in
  let generate_c = B0_env.in_scope_dir env ~/"lib/conf/generate.c" in
  let generate_exe = B0_env.in_scope_dir env ~/"lib/conf/generate.exe" in
  set generate_exe;
  let lib = B0_env.in_scope_dir env ~/"lib" in
  Os.Cmd.run Cmd.(cc_exe % "-I" %% path lib %% path generate_c % "-o" %% path generate_exe)

let miou_poll_config_src =
  let pp_value = Fmt.any "<miou_poll_config.ml>" in
  B0_meta.Key.make "miou_poll_config" ~doc:"Generated miou_poll_config.ml" ~pp_value

let miou_poll_config =
  let p, set = Fut.make () in
  let meta =
    B0_meta.empty
    |> B0_meta.add miou_poll_config_src p in
  let doc = "Generation of miou_poll_config.ml" in
  B0_unit.of_action ~doc ~meta ~units:[generate_exe] "miou_poll_config" @@ fun env _ ~args:_ ->
  let open Result.Syntax in
  let* generate_exe = B0_env.unit_exe_file_cmd env generate_exe in
  let output = B0_env.in_scope_dir env ~/"lib/miou_poll_config.ml" in
  let stdout = Os.Cmd.out_file ~force:true ~make_path:false output in
  let* () = Os.Cmd.run generate_exe ~stdout in
  set output; Ok ()

let miou_backoff_lib =
  let srcs = [ `File ~/"lib/miou_backoff.ml"; `File ~/"lib/miou_backoff.mli" ] in
  B0_ocaml.lib miou_backoff ~srcs

let miou_sync_lib =
  let srcs = [ `File ~/"lib/miou_sync.ml"; `File ~/"lib/miou_sync.mli" ] in
  let requires = [ miou_backoff ] in
  B0_ocaml.lib miou_sync ~srcs ~requires

let miou_lib =
  let srcs =
    [ `File ~/"lib/miou.mli"; `File ~/"lib/miou.ml"
    ; `File ~/"lib/miou_logs.mli"; `File ~/"lib/miou_logs.ml"
    ; `File ~/"lib/miou_pqueue.mli"; `File ~/"lib/miou_pqueue.ml"
    ; `File ~/"lib/miou_vector.mli"; `File ~/"lib/miou_vector.ml"
    ; `File ~/"lib/miou_sequence.mli"; `File ~/"lib/miou_sequence.ml"
    ; `File ~/"lib/miou_queue.mli"; `File ~/"lib/miou_queue.ml"
    ; `File ~/"lib/miou_state.mli"; `File ~/"lib/miou_state.ml"
    ; `File ~/"lib/miou_fmt.mli"; `File ~/"lib/miou_fmt.ml"
    ; `File ~/"lib/miou_gen.mli"; `File ~/"lib/miou_gen.ml"
    ]
  in
  let requires = [ miou_backoff; miou_sync ] in
  B0_ocaml.lib miou ~doc:"The Miou library" ~srcs ~requires

let miou_unix_lib =
  let miou_poll_config build =
    B0_build.require_unit build miou_poll_config;
    let m = B0_build.memo build in
    let miou_poll_config =
      B0_unit.get_meta miou_poll_config_src miou_poll_config
      |> B0_memo.fail_if_error m in
    Fut.map Fpath.Set.singleton miou_poll_config in
  let srcs =
    [ `File ~/"lib/miou_unix.mli"; `File ~/"lib/miou_unix.ml"
    ; `File ~/"lib/miou_bitv.mli"; `File ~/"lib/miou_bitv.ml"
    ; `File ~/"lib/miou_poll.mli"; `File ~/"lib/miou_poll.ml"
    ; `Fut miou_poll_config ] in
  let requires = [ unix; miou ] in
  B0_ocaml.lib miou_unix ~doc:"The Miou unix library" ~srcs ~requires

let default =
  B0_pack.make "miou" ~doc:"The Miou package" ~locked:true @@ B0_unit.list ()
