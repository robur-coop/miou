module Fuchsia = Miou_trace.Fuchsia

let run fn =
  let buf = Buffer.create 0x100 in
  let open Effect.Deep in
  let rec retc x = x
  and exnc exn = raise exn
  and effc : type c. c Effect.t -> ((c, 'a) continuation -> 'b) option =
    function
    | Fuchsia.Write str ->
        Some (fun k -> continue k (Buffer.add_string buf str))
    | _ -> None
  and handler = { retc; exnc; effc } in
  Buffer.add_string buf Fuchsia.magic_number;
  match_with fn (Fuchsia.create ()) handler;
  Buffer.contents buf

module Hex = struct
  let string_fold f acc str =
    let st = ref acc in
    String.iter (fun c -> st := f !st c) str;
    !st

  let is_space = function ' ' | '\n' | '\r' | '\t' -> true | _ -> false

  let digit = function
    | '0' .. '9' as c -> int_of_char c - 0x30
    | 'A' .. 'F' as c -> int_of_char c - 0x41 + 10
    | 'a' .. 'f' as c -> int_of_char c - 0x61 + 10
    | _ -> invalid_arg "bad character"

  let required_length ?(skip_whitespace = true) src =
    let req =
      string_fold
        (fun r c ->
          if skip_whitespace && is_space c then r
          else (
            ignore (digit c);
            succ r))
        0 src
    in
    if req mod 2 = 0 then req / 2 else invalid_arg "leftover byte in hex string"

  let decode_into ?(skip_whitespace = true) src tgt ?(off = 0) () =
    let fold f acc str =
      let st = ref acc in
      String.iter (fun c -> st := f !st c) str;
      !st
    in
    let chars, leftover =
      fold
        (fun (chars, leftover) c ->
          if skip_whitespace && is_space c then (chars, leftover)
          else
            let c = digit c in
            match leftover with
            | None -> (chars, Some (c lsl 4))
            | Some c' -> ((c' lor c) :: chars, None))
        ([], None) src
    in
    let chars = List.rev chars in
    if leftover <> None then invalid_arg "leftover byte in hex string";
    List.iteri (fun idx c -> Bytes.set_uint8 tgt (off + idx) c) chars

  let decode ?(skip_whitespace = true) src =
    let len = required_length ~skip_whitespace src in
    let buf = Bytes.create len in
    decode_into ~skip_whitespace src buf ();
    Bytes.unsafe_to_string buf
end

let test01 =
  let description = {text|duration_begin|text} in
  Test.test ~title:"test01" ~description @@ fun () ->
  let fn t =
    Fuchsia.duration_begin t ~name:"ring_start" ~category:"ocaml"
      ~thread:{ Fuchsia.pid= 0; tid= 0 } ~ts:0L
  in
  let output = run fn in
  let expected =
    {hex|1000 0446 7854 1600 2200 0100 0500 0000
     6f63 616d 6c00 0000 3200 0200 0a00 0000
     7269 6e67 5f73 7461 7274 0000 0000 0000
     3300 0100 0000 0000 0000 0000 0000 0000
     0000 0000 0000 0000 2400 0201 0100 0200
     0000 0000 0000 0000 |hex}
  in
  let expected = Hex.decode expected in
  Test.check (String.equal expected output)

let () =
  let tests = [ test01 ] in
  let ({ Test.directory } as runner) =
    Test.runner (Filename.concat (Sys.getcwd ()) "_tests")
  in
  let run idx test =
    let idx = succ idx in
    Format.printf "test%02d: %!" idx;
    Test.run runner test;
    Format.printf "ok\n%!"
  in
  Format.printf "Run tests into %s\n%!" directory;
  List.iteri run tests
