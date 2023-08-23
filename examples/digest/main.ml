exception File_not_found of string

let with_ic filename fn =
  if not (Sys.file_exists filename) then raise (File_not_found filename);
  try
    let ic = open_in filename in
    let rs = Fun.protect ~finally:(fun () -> close_in ic) (fn ic) in
    Ok rs
  with
  | Fun.Finally_raised exn -> raise exn
  | exn -> Error exn

let rec digest ctx buf ic () =
  match input ic buf 0 (Bytes.length buf) with
  | 0 | (exception End_of_file) -> Digestif.SHA256.get ctx
  | len ->
      let ctx = Digestif.SHA256.feed_bytes ctx buf ~off:0 ~len in
      digest ctx buf ic ()

let digest filename =
  with_ic filename (digest Digestif.SHA256.empty (Bytes.create 0x100))
  |> Result.map (fun hash -> (filename, hash))

let output = Mutex.create ()

let print result =
  Mutex.lock output;
  begin
    match result with
    | Ok (filename, hash) ->
        Format.printf "%s: %a\n%!" filename Digestif.SHA256.pp hash
    | Error (filename, File_not_found _) ->
        Format.printf "%s: No such file\n%!" filename
    | Error (filename, exn) ->
        Format.printf "%s: %S\n%!" filename (Printexc.to_string exn)
  end;
  Mutex.unlock output

let filenames_of_stdin () =
  let rec go acc =
    match input_line stdin with
    | exception End_of_file -> List.rev acc
    | filename -> go (filename :: acc)
  in
  go []

let run filenames =
  Miou.parallel digest filenames
  |> List.map Result.join
  |> List.mapi (fun idx ->
         Result.map_error (fun exn -> (List.nth filenames idx, exn)))
  |> List.iter print

let ( $ ) f g x = f (g x)

let () =
  match Sys.argv with
  | [| _ |] -> Miou.run (run $ filenames_of_stdin)
  | _ ->
      let filenames = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
      let filenames = Array.to_list filenames in
      Miou.run ~domains:2 @@ fun () -> run filenames
