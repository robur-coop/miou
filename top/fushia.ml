(*
module Reader = struct
  type _ Effect.t += Shift : int -> unit t
  type _ Effect.t += Get_int64 : int64 t
  type _ Effect.t += Get_string : int -> string t
  type _ Effect.t += Ensure : int -> unit t
  type _ Effect.t += Record_string : int * string -> unit t
  type _ Effect.t += Record_thread : int64 * int64 -> unit t

  let get_optional_string idx =
    if idx == 0 then String.empty
    else if idx land 0x8000 == 0 then
      find_string idx
    else
      let str_len = idx land 0x7fff in
      let str = get_string str_len in
      let padded = ((str_len + 7) lsr 3) lsl 3 in
      let () = shift padded in
      str

  let get_optional_thread idx =
    if idx == 0 then
      let pid = get_int64 () in
      let ()  = shift 8 in
      let tid = get_int64 () in
      let ()  = shift 8 in
      (pid, tid)
    else find_thread idx

  let get_record () =
    let hdr = get_int64 () in
    let low = Int64.to_int hdr 0x7fffffff in
    let tyr = low land 0xf in
    let len = ((low lsr 4) land 0xfff) * 8 in
    let ()  = ensure len in
    let ()  = shift 8 in
    match tyr with
    | 0 -> Some Metadata
    | 1 -> ...
    | 2 ->
      let idx = low lsr 16 in
      let str_len = Int64.(to_int (shift_right hdr 32)) land 0x7fff in
      let str = get_string str_len in
      assert (len = str_len + 8);
      record_string idx str;
      shift str_len
    | 3 ->
      let idx = (low lsr 16) land 0xff in
      let pid = get_int64 () in
      let ()  = shift 8 in
      let tid = get_int64 () in
      let ()  = shift 8 in
      record_thread idx ~pid ~tid
    | 4 ->
      let hlow = Int64.(to_int (shift_right hdr 16)) land 0xffff in
      let event = hlow land 0xf
      and nargs = (hlow lsr 4) land 0xf
      and idx0 = (hlow lsr 8) land 0xff
      and idx1 = Int64.(to_int (shift_right hdr 32)) land 0xffff
      and idx2 = Int64.(to_int (shift_right hdr 48)) in
      let timestamp = get_int64 () in
      let () = shift 8 in
      let pid, tid = get_optional_thread idx0 in
      let category = get_optional_string idx1 in
      let name = get_optional_string idx2 in
end

module I64 = struct
  let ( lsl ) = Int64.shift_left
  let ( lsr ) = Int64.shift_right_logical
  let ( lor ) = Int64.logor
end

type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external bigarray_set_int64_ne : bigstring -> int -> int64 -> unit = "%caml_bigstring_set64u"

module Fmt = struct
  type 'a atom =
    | Int64 : int64 atom
    | Int32 : int32 atom
    | Float : float atom
    | Pointer : int64 atom
    | Koid : int64 atom
    | String : string atom
    | Bool : bool atom

  type string_ref =
    | Ref of int
    | String of string

  let bool_to_string buf ~off ~name value = match name, value with
    | true, Ref idx ->
        let hdr = (8L lsl 4) + 9L in
        let idx = (of_int idx) lsl 16 in
        let bol = 1L lsl 32 in
        bigarray_set_int64_le buf (bol lor idx lor hdr);
        8
    | true, String str ->
        let padded = ((String.length str + 7) lsr 3) lsl 3 in
        let hdr = (8L lsl 4) + 9L in
        let idx = ((of_int (String.length str)) lor 0x8000) lsl 16 in
        let bol = 1L lsl 32 in
        bigarray_set_int64_le buf (bol lor idx lor hdr);



  type ('ty, 'v) fmt =
    | [] : ('v, 'v) fmt
    | ( :: ) : 'a atom * ('ty, 'v) fmt -> (string -> 'a -> 'ty, 'v) fmt

  let rec concat
  : type a b c. (a, b) fmt -> (b, c) fmt -> (a, c) fmt
  = fun a b -> match a with
    | [] -> b
    | x :: a' -> x :: concat a' b

  let unit = Unit
  let int64 = Int64
  let ptr = Pointer
  let koid = Koid
  let str = String
end

let _ = Fmt.to_string Fmt.[ unit; koid ] "unit" () "process" 0L
*)
