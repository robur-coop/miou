(* Copyright (c) 2016 The fmt programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*)

let pf = Format.fprintf
let str = Format.asprintf
let kstr = Format.kasprintf
let failwith fmt = kstr failwith fmt
let invalid_arg fmt = kstr invalid_arg fmt
let using f pp ppf v = pp ppf (f v)
let any fmt ppf _ = pf ppf fmt
let fmt fmt ppf = pf ppf fmt

type 'a t = Format.formatter -> 'a -> unit

let cut ppf _ = Format.pp_print_cut ppf ()
let string ppf str = Format.pp_print_string ppf str
let int ppf n = Format.pp_print_int ppf n

let iter ?sep:(pp_sep = cut) iter pp_elt ppf v =
  let is_first = ref true in
  let pp_elt v =
    if !is_first then is_first := false else pp_sep ppf ();
    pp_elt ppf v
  in
  iter pp_elt v

let space ppf _ = Format.pp_print_space ppf ()

let comma ppf _ =
  Format.pp_print_string ppf ",";
  space ppf ()

let semi ppf _ =
  Format.pp_print_string ppf ";";
  space ppf ()

let append pp_v0 pp_v1 ppf v = pp_v0 ppf v; pp_v1 ppf v
let ( ++ ) = append
let concat ?sep pps ppf v = iter ?sep List.iter (fun ppf pp -> pp ppf v) ppf pps
let list ?sep pp_elt = iter ?sep List.iter pp_elt

let surround s1 s2 pp_v ppf v =
  let open Format in
  pp_print_string ppf s1; pp_v ppf v; pp_print_string ppf s2

let box ?(indent = 0) pp_v ppf v =
  let open Format in
  pp_open_box ppf indent; pp_v ppf v; pp_close_box ppf ()

let parens pp_v = box ~indent:1 (surround "(" ")" pp_v)
let brackets pp_v = box ~indent:1 (surround "[" "]" pp_v)

let iter_bindings ?sep:(pp_sep = cut) iter pp_binding ppf v =
  let is_first = ref true in
  let pp_binding k v =
    if !is_first then is_first := false else pp_sep ppf ();
    pp_binding ppf (k, v)
  in
  iter pp_binding v

module Dump = struct
  let iter f pp_name pp_elt =
    let pp_v = iter ~sep:space f (box pp_elt) in
    parens (pp_name ++ space ++ pp_v)

  let pair pp_fst pp_snd =
    parens (using fst (box pp_fst) ++ comma ++ using snd (box pp_snd))

  let iter_bindings f pp_name pp_k pp_v =
    let pp_v = iter_bindings ~sep:space f (pair pp_k pp_v) in
    parens (pp_name ++ space ++ pp_v)

  let list pp_elt = brackets (list ~sep:semi (box pp_elt))
  let hashtbl pp_k pp_v = iter_bindings Hashtbl.iter (any "hashtbl") pp_k pp_v

  let option pp_value ppf = function
    | None -> pf ppf "None"
    | Some value -> pf ppf "@[<2>Some@ @[%a@]@]" pp_value value
end
