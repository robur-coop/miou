let pf = Format.fprintf
let str = Format.asprintf
let kstr = Format.kasprintf
let failwith fmt = kstr failwith fmt
let invalid_arg fmt = kstr invalid_arg fmt

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

module Dump = struct
  let iter f pp_name pp_elt =
    let pp_v = iter ~sep:space f (box pp_elt) in
    parens (pp_name ++ space ++ pp_v)

  let list pp_elt = brackets (list ~sep:semi (box pp_elt))
end
