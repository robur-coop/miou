let invalid_arg fmt = Format.kasprintf invalid_arg fmt

let load filename =
  let ic = open_in filename in
  let ln = in_channel_length ic in
  let rs = Bytes.create ln in
  really_input ic rs 0 ln; close_in ic; Bytes.unsafe_to_string rs

let () =
  let system, output =
    match Sys.argv with
    | [| _; "--system"; system; "-o"; output |] ->
        let system =
          match system with
          | "linux" | "elf" -> `Linux
          | "win32" | "win64" | "windows" | "mingw64" | "mingw" | "cygwin" ->
              `Windows
          | "freebsd" -> `FreeBSD
          | "macosx" -> `MacOSX
          | v when String.length v >= 5 ->
              if String.sub system 0 5 = "linux" then `Linux
              else invalid_arg "Invalid system %S" v
          | v -> invalid_arg "Invalid system %S" v
        in
        (system, output)
    | _ -> invalid_arg "%s --system system -o <output>" Sys.argv.(0)
  in
  let oc_ml, oc_cc, oc_sexp =
    ( open_out (output ^ ".ml")
    , open_out (output ^ "_stubs.c")
    , open_out (output ^ ".sexp") )
  in
  let str_ml, str_cc, str_sexp =
    match system with
    | `Linux -> (load "clock.linux.ml", load "clock.linux.c", "(-lrt)")
    | `MacOSX -> (load "clock.mach.ml", load "clock.mach.c", "()")
    | `Windows -> (load "clock.windows.ml", load "clock.windows.c", "()")
    | `FreeBSD -> (load "clock.linux.ml", load "clock.linux.c", "()")
  in
  output_string oc_ml str_ml;
  output_string oc_cc str_cc;
  output_string oc_sexp str_sexp;
  close_out oc_ml;
  close_out oc_cc;
  close_out oc_sexp
