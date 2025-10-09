open Std

let run includes input output =
  let c_compiler = Conf.(config.%{"c_compiler"}) in
  let native_cflags = Conf.find_as_args "native_cflags" in
  let native_cppflags = Conf.find_as_args "native_cppflags" in
  let native_c_libraries = Conf.find_as_args "native_c_libraries" in
  let includes = List.map (fun x -> [ "-I"; x ]) includes in
  let includes = List.flatten includes in
  let cmd =
    let open Cmd in
    v c_compiler
    %% native_cflags
    %% native_cppflags
    %% includes
    %% input
    % "-o"
    % output
    %% native_c_libraries
  in
  let* status = Exec.status cmd in
  match status with
  | `Exited 0 -> Ok ()
  | `Exited n ->
      error_msgf
        "Impossible to compile C source files (C compiler exited with %d)" n

let usage = "cc <file1> [<file2>] -o <output>"
let input_files = ref []
let output_file = ref ""
let includes = ref []
let extend v str = v := str :: !v
let anon filename = input_files := filename :: !input_files

let spec =
  [
    ("-o", Arg.Set_string output_file, "Set output filename")
  ; ("-I", Arg.String (extend includes), "Include a directory")
  ]

let () =
  Arg.parse spec anon usage;
  let input = !input_files and output = !output_file and includes = !includes in
  match run includes input output with
  | Ok () -> ()
  | Error (`Msg msg) -> failwith msg
