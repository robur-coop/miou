#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let miou_opam = Pkg.opam_file "opam"

let () =
  Pkg.describe ~opams:[ miou_opam ] "miou" @@ fun c ->
  Ok
    [ Pkg.mllib "lib/miou_backoff.mllib"
    ; Pkg.mllib "lib/miou_sync.mllib"
    ; Pkg.mllib "lib/miou.mllib"
    ; Pkg.mllib "lib/miou_unix.mllib" ]
