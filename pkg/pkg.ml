#!/usr/bin/env ocaml

[@@@ocamlformat "disable"]

#use "topfind"
#require "topkg"

open Topkg

let miou_opam = Pkg.opam_file "miou.opam"

let () =
  Pkg.describe ~opams:[ miou_opam ] "miou" @@ fun c ->
  Ok
    [
      Pkg.mllib ~dst_dir:"backoff/" "lib/miou_backoff.mllib"
    ; Pkg.mllib ~dst_dir:"sync/" "lib/miou_sync.mllib"
    ; Pkg.mllib ~dst_dir:"runtime_events/" "lib/miou_runtime_events.mllib"
    ; Pkg.mllib "lib/miou.mllib"
    ; Pkg.mllib ~dst_dir:"unix/" "lib/miou_unix.mllib"
    ; Pkg.test ~dir:"test" "test/test_core"
    ]
