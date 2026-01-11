open Std

let output has_ppoll =
  {c|#ifndef TEST_MIOU_POLL_H
#define TEST_MIOU_POLL_H
|c}
  ^ (if has_ppoll then "#define HAS_PPOLL" else "")
  ^ {c|
#endif
|c}

let run filename =
  let c_compiler = Conf.(config.%{"c_compiler"}) in
  let native_cflags = Conf.find_as_args "native_cflags" in
  let native_cppflags = Conf.find_as_args "native_cppflags" in
  let native_c_libraries = Conf.find_as_args "native_c_libraries" in
  let* out = File.tmp () in
  let cmd =
    let open Cmd in
    v c_compiler
    %% native_cflags
    %% native_cppflags
    % "-c"
    % filename
    % "-o"
    % out
    %% native_c_libraries
  in
  let* status = Exec.status cmd in
  let has_ppoll = match status with `Exited 0 -> true | `Exited _ -> false in
  File.write "-" (output has_ppoll)

let () =
  match run Sys.argv.(1) with Ok () -> () | Error (`Msg msg) -> failwith msg
