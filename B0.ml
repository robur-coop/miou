open B0_kit.V000

let unix = B0_ocaml.libname "unix"
let miou = B0_ocaml.libname "miou"
let miou_unix = B0_ocaml.libname "miou.unix"
let miou_backoff = B0_ocaml.libname "miou.backoff"
let miou_sync = B0_ocaml.libname "miou.sync"

let miou_backoff_lib =
  let srcs = [ `File ~/"lib/backoff.ml"; `File ~/"lib/backoff.mli" ] in
  let requires = [] in
  B0_ocaml.lib miou_backoff ~srcs ~requires

let miou_sync_lib =
  let srcs = [ `File ~/"lib/sync.ml"; `File ~/"lib/sync.mli" ] in
  let requires = [ miou_backoff ] in
  B0_ocaml.lib miou_sync ~srcs ~requires

let[@ocamlformat "disable"] miou_lib =
  let srcs =
    [
      `File ~/"lib/miou.mli"; `File ~/"lib/miou.ml"
    ; `File ~/"lib/logs.mli"; `File ~/"lib/logs.ml"
    ; `File ~/"lib/heapq.mli"; `File ~/"lib/heapq.ml"
    ; `File ~/"lib/sequence.mli"; `File ~/"lib/sequence.ml"
    ; `File ~/"lib/queue.mli"; `File ~/"lib/queue.ml"
    ; `File ~/"lib/state.mli"; `File ~/"lib/state.ml"
    ; `File ~/"lib/fmt.mli"; `File ~/"lib/fmt.ml"
    ; `File ~/"lib/gen.mli"; `File ~/"lib/gen.ml"
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
