let out filename =
  let ic = open_in_bin filename in
  let finally () = close_in ic in
  Fun.protect ~finally @@ fun () ->
  let buf = Bytes.create 0x7ff in
  let rec go () =
    let len = input ic buf 0 (Bytes.length buf) in
    match len with
    | 0 | (exception End_of_file) -> ()
    | len ->
        let str = Bytes.sub_string buf 0 len in
        output_string stdout str; go ()
  in
  go ()

let () =
  let filenames = Array.to_list Sys.argv in
  let filenames = List.tl filenames in
  List.iter out filenames
