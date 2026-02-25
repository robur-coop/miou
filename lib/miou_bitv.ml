(* Copyright (c) Jean-Christophe Filliatre
   SPDX-License-Identifier: MIT
   Copyright (c) 2025 Romain Calascibetta <romain.calascibetta@gmail.com> *)

(* Bit vectors. The interface and part of the code are borrowed from the
   [Array] module of the OCaml standard library (but things are simplified
   here since we can always initialize a bit vector). This module also
   provides bitwise operations. *)

(* We represent a bit vector by a string of bytes (field [bits]), and
   we keep the information of the size of the bit vector (field
   [length]) since it can not be figured out with the size of the
   array. *)

external bytes_unsafe_set : bytes -> int -> int -> unit = "%bytes_unsafe_set"
external bytes_unsafe_get : bytes -> int -> int = "%bytes_unsafe_get"

type t = { length: int; bits: bytes; mutable hi: int }

let length { length; _ } = length
let[@inline] equal (v1 : t) (v2 : t) = v1 = v2

let max_length =
  if max_int lsr 3 < Sys.max_string_length then max_int
  else Sys.max_string_length * 8

let _exceeds_max_length n =
  let s = n / 8 in
  (if n land 0b111 = 0 then s else s + 1) / Sys.max_string_length

let low_mask = Array.init 9 (fun i -> (1 lsl i) - 1)

let create n b =
  if n < 0 || n > max_length then invalid_arg "Miou_unix.Bitv.create";
  let initv = if b then 255 else 0 in
  let q = n lsr 3 and r = n land 7 in
  if r == 0 then
    let bits = Bytes.make q (Char.chr initv) in
    { length= n; bits; hi= if b then n else 0 }
  else begin
    let s = Bytes.make (q + 1) (Char.chr initv) in
    bytes_unsafe_set s q (initv land low_mask.(r));
    { length= n; bits= s; hi= if b then n else 0 }
  end

let unsafe_get v n =
  let i = n lsr 3 in
  bytes_unsafe_get v.bits i land (1 lsl (n land 7)) > 0

let get v n =
  if n < 0 || n >= v.length then invalid_arg "Miou_unix.Bitv.get";
  unsafe_get v n

external miou_bitv_clz : bytes -> (int[@untagged])
  = "miou_bitv_clz_bytecode" "miou_bitv_clz_native"
[@@noalloc]

let unsafe_set v n b =
  let i = n lsr 3 in
  let c = bytes_unsafe_get v.bits i in
  let mask = 1 lsl (n land 7) in
  bytes_unsafe_set v.bits i (if b then c lor mask else c land lnot mask)

let set v n b =
  if n < 0 || n >= v.length then invalid_arg "Miou_unix.Bitv.set";
  unsafe_set v n b;
  if b then (if n + 1 > v.hi then v.hi <- n + 1)
  else if n + 1 = v.hi then v.hi <- miou_bitv_clz v.bits

let ntz = Array.make 256 0

let () =
  for i = 0 to 7 do
    ntz.(1 lsl i) <- i
  done

let ntz8 x = Array.unsafe_get ntz x

let rec _visit ~fn idx x =
  if x != 0 then begin
    let b = x land -x in
    fn (idx + ntz8 b);
    _visit ~fn idx (x - b)
  end

let iter fn v =
  for i = 0 to Bytes.length v.bits - 1 do
    let c = bytes_unsafe_get v.bits i in
    let idx = i lsl 3 in
    _visit ~fn idx c
  done

external miou_bitv_next : bytes -> (int[@untagged])
  = "miou_bitv_next_bytecode" "miou_bitv_next_native"
[@@noalloc]

let next v =
  let n = miou_bitv_next v.bits in
  if n < v.length then Some n else None

let max v = v.hi
