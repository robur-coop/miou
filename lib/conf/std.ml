let error_msg msg = Error (`Msg msg)
let error_msgf fmt = Format.kasprintf error_msg fmt
let reword_error fn = function Ok _ as v -> v | Error err -> Error (fn err)
let dash = "-"
let ( let* ) = Result.bind

external reraise : exn -> 'a = "%reraise"

module Env = struct
  let var name = try Some (Sys.getenv name) with Not_found -> None
end

module Dir = struct
  let exists dirname =
    try Ok Sys.(file_exists dirname && is_directory dirname)
    with Sys_error err -> error_msgf "%s: %s" dirname err

  let must_exist dirname =
    let* exists = exists dirname in
    if exists then Ok dirname else error_msgf "%s: no such directory" dirname
end

module File = struct
  let with_parent_check op name filename =
    let err_no_parent name filename =
      Printf.sprintf "%s: Cannot %s file, parent directory does not exist"
        filename name
    in
    let v = Dir.must_exist (Filename.dirname filename) in
    let* _ = reword_error (fun _ -> `Msg (err_no_parent name filename)) v in
    Ok (op filename)

  let open_in_bin = with_parent_check open_in_bin "read"
  let open_out_bin = with_parent_check open_out_bin "write"

  let read filename =
    try
      let close ic = if filename = dash then () else close_in_noerr ic in
      let* ic = if filename = dash then Ok stdin else open_in_bin filename in
      try
        let len = in_channel_length ic in
        let buf = Bytes.create len in
        really_input ic buf 0 len;
        close ic;
        Ok (Bytes.unsafe_to_string buf)
      with exn -> close ic; reraise exn
    with Sys_error err -> error_msgf "%s: %s" filename err

  let delete ?(must_exist = false) filename =
    try
      if (not must_exist) && not (Sys.file_exists filename) then Ok ()
      else Ok (Sys.remove filename)
    with Sys_error err -> error_msgf "%s: %s" filename err

  let write filename contents =
    try
      let close oc = if filename = dash then () else close_out_noerr oc in
      let* oc = if filename = dash then Ok stdout else open_out_bin filename in
      try output_string oc contents; flush oc; close oc; Ok ()
      with exn -> close oc; reraise exn
    with Sys_error err -> error_msgf "%s: %s" filename err

  let tmp () =
    try
      let filename =
        Filename.temp_file (Filename.basename Sys.argv.(0)) "miou"
      in
      at_exit (fun () -> ignore (delete filename));
      Ok filename
    with Sys_error err -> error_msg err
end

module Cmd = struct
  type t = string list

  let empty = []
  let is_empty = function [] -> true | _ -> false
  let v a = [ a ]
  let ( % ) l a = a :: l
  let ( %% ) (l0 : t) (l1 : t) = List.rev_append l1 l0
  let to_rev_list l = l

  let dump ppf cmd =
    let pp_elt ppf v = Format.fprintf ppf "%s" (Filename.quote v) in
    let rec go = function
      | [] -> ()
      | hd :: tl ->
          if tl = [] then pp_elt ppf hd
          else (
            Format.fprintf ppf "%a@ " pp_elt hd;
            go tl)
    in
    Format.fprintf ppf "@[<1>[";
    go (List.rev cmd);
    Format.fprintf ppf "]@]"
end

module Exec = struct
  let line ?stdout ?stderr cmd =
    let strf = Printf.sprintf in
    if Cmd.is_empty cmd then invalid_arg "no command, empty command line";
    let cmd = List.rev_map Filename.quote (Cmd.to_rev_list cmd) in
    let cmd = String.concat " " cmd in
    let redirect fd filename = strf " %d>%s" fd (Filename.quote filename) in
    let stdout = Option.fold ~none:"" ~some:(redirect 1) stdout in
    let stderr = Option.fold ~none:"" ~some:(redirect 2) stderr in
    let win_quote = if Sys.win32 then "\"" else "" in
    strf "%s%s%s%s%s" win_quote cmd stdout stderr win_quote

  let exec ?stdout ?stderr cmd =
    try
      let line = line ?stdout ?stderr cmd in
      Ok ((), (cmd, `Exited (Sys.command line)))
    with Sys_error err | Failure err -> error_msg err

  type out = { cmd: Cmd.t; err: string option }

  let out ?err cmd = { cmd; err }

  let out_string ?(trim = true) t =
    let ( let* ) = Result.bind in
    let* filename = File.tmp () in
    let* (), status = exec ?stderr:t.err ~stdout:filename t.cmd in
    let* out = File.read filename in
    Ok ((if trim then String.trim out else out), status)

  let out_lines ?trim t =
    let* v, status = out_string ?trim t in
    let v = if v = "" then [] else String.split_on_char '\n' v in
    Ok (v, status)

  let success = function
    | Error _ as err -> err
    | Ok (v, (_, `Exited 0)) -> Ok v
    | Ok (_, (cmd, `Exited x)) ->
        error_msgf "cmd %a: exited with %d" Cmd.dump cmd x

  let to_lines ?trim t = out_lines ?trim t |> success

  let status ?err:stderr cmd =
    let* (), (_, status) = exec ?stderr cmd in
    Ok status
end

module Conf = struct
  let config () =
    let ocamlc = Cmd.v "ocamlc" in
    let* lines = Exec.(out Cmd.(ocamlc % "-config") |> to_lines) in
    let parse_line acc line =
      match String.split_on_char ':' line with
      | [] | [ _ ] -> acc
      | x :: r ->
          let value = String.concat ":" r in
          let value = String.trim value in
          (x, value) :: acc
    in
    let conf = List.(rev (fold_left parse_line [] lines)) in
    Ok conf

  let config =
    match config () with
    | Ok config -> config
    | Error (`Msg msg) -> failwith msg

  let ( .%{} ) lst key = List.assoc key lst

  let find_as_args key =
    let args = String.split_on_char ' ' config.%{key} in
    let fn = function "" -> None | x -> Some x in
    List.filter_map fn args
end
