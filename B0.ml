open B0_kit.V000

[@@@ocamlformat "disable"]

let ( ~/ ) x = Fpath.v x
let unix = B0_ocaml.libname "unix"
let miou = B0_ocaml.libname "miou"
let miou_unix = B0_ocaml.libname "miou.unix"
let miou_backoff = B0_ocaml.libname "miou.backoff"
let miou_sync = B0_ocaml.libname "miou.sync"

let miou_backoff_lib =
  let srcs = [ `File ~/"lib/miou_backoff.ml"; `File ~/"lib/miou_backoff.mli" ] in
  let requires = [] in
  B0_ocaml.lib miou_backoff ~srcs ~requires

let miou_sync_lib =
  let srcs = [ `File ~/"lib/miou_sync.ml"; `File ~/"lib/miou_sync.mli" ] in
  let requires = [ miou_backoff ] in
  B0_ocaml.lib miou_sync ~srcs ~requires

let[@ocamlformat "disable"] miou_lib =
  let srcs =
    [
      `File ~/"lib/miou.mli"; `File ~/"lib/miou.ml"
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
  let srcs = [ `File ~/"lib/miou_unix.mli"; `File ~/"lib/miou_unix.ml" ] in
  let requires = [ unix; miou ] in
  B0_ocaml.lib miou_unix ~doc:"The Miou unix library" ~srcs ~requires

let default =
  B0_pack.make "miou" ~doc:"The Miou package" ~locked:true @@ B0_unit.list ()
