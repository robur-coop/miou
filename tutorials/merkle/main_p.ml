module Hash = Digestif.SHA1

let ( / ) = Filename.concat

let hash_of_blob filename =
  let ic = open_in filename in
  let ln = in_channel_length ic in
  let rec go buf ctx =
    match input ic buf 0 (Bytes.length buf) with
    | 0 | (exception End_of_file) -> Hash.get ctx
    | len ->
        let ctx = Hash.feed_bytes ctx buf ~len in
        go buf ctx
  in
  let ctx = Hash.empty in
  let str = Fmt.str "blob %d\000" ln in
  let ctx = Hash.feed_string ctx str in
  let res = go (Bytes.create 0x1000) ctx in
  close_in ic; res

let empty_tree = Hash.digest_string "tree 0\000"

let rec hash_of_tree filename =
  let entries = Sys.readdir filename in
  let entries =
    List.map
      (fun v ->
        let filename = filename / v in
        if Sys.is_directory filename then (`Dir, filename)
        else (`Normal, filename))
      (List.sort String.compare (Array.to_list entries))
  in
  hash_of_entries entries

and perform = function
  | `Dir, filename ->
      let name = Filename.basename filename in
      let hash = hash_of_tree filename in
      Fmt.str "40000 %s\000%s" name (Hash.to_raw_string hash)
  | `Normal, filename ->
      let name = Filename.basename filename in
      let hash = hash_of_blob filename in
      Fmt.str "100644 %s\000%s" name (Hash.to_raw_string hash)

and hash_of_entries = function
  | [] -> empty_tree
  | hd :: tl ->
      let hd = Miou.call_cc @@ fun () -> perform hd in
      let entries = Miou.await hd :: Miou.parallel perform tl in
      let entries = List.map Result.get_ok entries in
      let ctx = Hash.empty in
      let len =
        List.fold_left (fun acc str -> acc + String.length str) 0 entries
      in
      let str = Fmt.str "tree %d\000" len in
      let ctx = Hash.feed_string ctx str in
      let ctx =
        List.fold_left (fun ctx str -> Hash.feed_string ctx str) ctx entries
      in
      Hash.get ctx

let run fn arg = Miou.run @@ fun () -> fn arg

let () =
  match Sys.argv with
  | [| _; filename |] when Sys.file_exists filename ->
      if Sys.is_directory filename then
        let hash = run hash_of_tree filename in
        Format.printf "%a\n%!" Hash.pp hash
      else
        let hash = run hash_of_blob filename in
        Format.printf "%a\n%!" Hash.pp hash
  | [| _; filename |] ->
      Format.eprintf "%s: %s not found\n%!" Sys.argv.(0) filename
  | _ -> Format.eprintf "%s <filename>\n%!" Sys.argv.(0)
