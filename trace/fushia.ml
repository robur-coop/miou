type _ Effect.t += Write : string -> unit Effect.t

let write str = Effect.perform (Write str)

let word value =
  let buf = Bytes.create 8 in
  Bytes.set_int64_le buf 0 value;
  write (Bytes.unsafe_to_string buf)

let record ?(data = 0) words t =
  let buf = Bytes.create 8 in
  let data = Int64.of_int (data land 0xffffffffffff) in
  let data = Int64.shift_left data 16 in
  Bytes.set_int64_le buf 0 data;
  let rem = ((words land 0xfff) lsl 4) lor (t land 0b1111) in
  Bytes.set_uint16_le buf 6 rem;
  write (Bytes.unsafe_to_string buf)

type strings = {
    mutable gen: int
  ; to_string: string option array
  ; to_index: (string, int) Hashtbl.t
}

type thread = { pid: int; tid: int }

type threads = {
    mutable gen: int
  ; to_thread: thread option array
  ; to_index: (thread, int) Hashtbl.t
}

type t = { strings: strings; threads: threads }

module String_ref = struct
  type t = Ref of int | Inline of string

  let strlen str = (String.length str + 7) / 8
  let words = function Ref _ -> 0 | Inline str -> strlen str

  let lookup t = function
    | "" -> Ref 0
    | str -> (
        match Hashtbl.find_opt t.strings.to_index str with
        | Some idx -> Ref idx
        | None -> Inline str)

  let write_padded str =
    write str;
    let rem = String.length str land 7 in
    if rem > 0 then write (String.make (8 - rem) '\000')

  let add t str =
    if String.length str > 32000 then
      invalid_arg "Fushia.String_ref.add: string too long";
    if not (Hashtbl.mem t.strings.to_index str) then begin
      let index = t.strings.gen + 1 in
      t.strings.gen <- (t.strings.gen + 1) land 0x7fff;
      let fn = Hashtbl.remove t.strings.to_index in
      Option.iter fn t.strings.to_string.(index);
      t.strings.to_string.(index) <- Some str;
      let words = strlen str + 1 in
      let data = index lor (String.length str lsl 16) in
      record ~data words 2; write_padded str
    end

  let write_inline = function Ref _ -> () | Inline str -> write_padded str

  let encode = function
    | Ref x -> x
    | Inline str -> 0x8000 lor String.length str
end

module Thread_ref = struct
  type t = Ref of int | Inline of thread

  let lookup t v =
    match Hashtbl.find_opt t.threads.to_index v with
    | Some idx -> Ref idx
    | None -> Inline v

  let encode = function Ref idx -> idx | Inline _ -> 0
  let words = function Ref _ -> 0 | Inline _ -> 2

  let write_inline = function
    | Ref _ -> ()
    | Inline { pid; tid } ->
        let buf = Bytes.create 16 in
        Bytes.set_int64_le buf 0 (Int64.of_int pid);
        Bytes.set_int64_le buf 8 (Int64.of_int tid);
        write (Bytes.unsafe_to_string buf)

  let add t v =
    if not (Hashtbl.mem t.threads.to_index v) then begin
      let idx = t.threads.gen + 1 in
      t.threads.gen <- (t.threads.gen + 1) land 0x7f;
      let fn = Hashtbl.remove t.threads.to_index in
      Option.iter fn t.threads.to_thread.(idx);
      t.threads.to_thread.(idx) <- Some v;
      Hashtbl.add t.threads.to_index v idx;
      record ~data:idx 3 3;
      let buf = Bytes.create 16 in
      Bytes.set_int64_le buf 0 (Int64.of_int v.pid);
      Bytes.set_int64_le buf 8 (Int64.of_int v.tid);
      write (Bytes.unsafe_to_string buf)
    end
end

module I64 = struct
  include Int64

  let ( <<< ) = shift_left
  let ( ||| ) = logor
  let i = of_int
end

module Arg = struct
  type ('u, 'v) t =
    | Unit : (unit, unit) t
    | Koid : (int64, int64) t
    | Ptr : (int64, int64) t
    | I64 : (int64, int64) t
    | String : (string, String_ref.t) t

  let lookup : type u v. _ -> (u, v) t -> u -> v =
   fun t w v ->
    match w with
    | Unit -> ()
    | Koid -> v
    | Ptr -> v
    | I64 -> v
    | String -> String_ref.lookup t v

  let add : type u v. _ -> (u, v) t -> u -> unit =
   fun t -> function
    | Unit | Koid | Ptr | I64 -> ignore
    | String -> String_ref.add t

  let words : type u v. (u, v) t -> v -> int = function
    | Unit -> Fun.const 0
    | Koid | Ptr | I64 -> Fun.const 1
    | String -> String_ref.words

  let header : type u v. (u, v) t -> v -> int = function
    | Unit | Koid | Ptr | I64 -> Fun.const 0
    | String -> String_ref.encode

  let kind : type u v. (u, v) t -> int = function
    | Unit -> 0
    | I64 -> 3
    | String -> 6
    | Ptr -> 7
    | Koid -> 8

  let write_inline : type u v. (u, v) t -> v -> unit = function
    | Unit -> ignore
    | Koid -> word
    | Ptr -> word
    | I64 -> word
    | String -> String_ref.write_inline
end

type arg = Arg : ('u, 'v) Arg.t * 'u -> arg
type recorded = Recorded : ('u, 'v) Arg.t * 'v -> recorded

let unit = Arg (Unit, ())
let koid v = Arg (Koid, v)
let ptr v = Arg (Ptr, v)
let i64 v = Arg (I64, v)
let str str = Arg (String, str)

type args = (string * arg) list

module Args = struct
  let add t =
    List.iter (fun (k, Arg (w, v)) -> String_ref.add t k; Arg.add t w v)

  let lookup t =
    let fn (k, Arg (w, v)) =
      let k = String_ref.lookup t k and v = Arg.lookup t w v in
      (k, Recorded (w, v))
    in
    List.map fn

  let words =
    let fn acc (k, Recorded (w, v)) =
      acc + 1 + String_ref.words k + Arg.words w v
    in
    List.fold_left fn 0

  let write =
    let fn (k, Recorded (w, v)) =
      let words = 1 + String_ref.words k + Arg.words w v in
      let value = Arg.header w v in
      let lo = Arg.kind w lor (words lsl 4) lor (String_ref.encode k lsl 16) in
      let value = I64.(i value <<< 32 ||| i lo) in
      word value; String_ref.write_inline k; Arg.write_inline w v
    in
    List.iter fn
end

let event t ?(args = []) ~name ~thread ~category ~ts kind =
  String_ref.add t category;
  String_ref.add t name;
  Thread_ref.add t thread;
  Args.add t args;
  let argc = List.length args in
  let name = String_ref.lookup t name in
  let category = String_ref.lookup t category in
  let thread = Thread_ref.lookup t thread in
  let args = Args.lookup t args in
  let words =
    2
    + Thread_ref.words thread
    + String_ref.words name
    + String_ref.words category
    + Args.words args
  in
  let data =
    kind
    lor (argc lsl 4)
    lor (Thread_ref.encode thread lsl 8)
    lor (String_ref.encode category lsl 16)
    lor (String_ref.encode name lsl 32)
  in
  record ~data words 4;
  word ts;
  Thread_ref.write_inline thread;
  String_ref.write_inline category;
  String_ref.write_inline name;
  Args.write args

let kernel_object t ?(args = []) ~name oid =
  Args.add t args;
  String_ref.add t name;
  let argc = List.length args in
  let args = Args.lookup t args in
  let name = String_ref.lookup t name in
  let words = 2 + String_ref.words name + Args.words args in
  let data = 2 lor (String_ref.encode name lsl 8) lor (argc lsl 24) in
  record ~data words 2; word oid; Args.write args

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
