let () = Printexc.record_backtrace true

let check test =
  let bt = Printexc.get_callstack max_int in
  try
    assert test;
    print_string ".";
    flush stdout
  with exn ->
    print_string "x";
    flush stdout;
    Printexc.raise_with_backtrace exn bt

let uid_gen = Atomic.make 1

type t = { title: string; description: string; fn: unit -> unit; uid: int }

let test ~title ~description fn =
  { title; description; fn; uid= Atomic.fetch_and_add uid_gen 1 }

type runner = { directory: string }

let rec mkdir_p path perm =
  if path <> "" then (
    try Unix.mkdir path perm with
    | Unix.Unix_error (EEXIST, _, _) when Sys.is_directory path -> ()
    | Unix.Unix_error (ENOENT, _, _) ->
        mkdir_p (Filename.dirname path) perm;
        Unix.mkdir path perm)

let mkdir ({ directory } as runner) = mkdir_p directory 0o755; runner

let runner ?(g = Random.State.make_self_init ()) ?(fmt = Miou.Fmt.fmt "run-%s")
    root =
  let random_string len =
    let res = Bytes.create len in
    for i = 0 to len - 1 do
      let chr =
        match Random.State.int g (26 + 26 + 10) with
        | n when n < 26 -> Char.chr (Char.code 'a' + n)
        | n when n < 26 + 26 -> Char.chr (Char.code 'A' + n - 26)
        | n -> Char.chr (Char.code '0' + n - 26 - 26)
      in
      Bytes.set res i chr
    done;
    Bytes.unsafe_to_string res
  in
  let rec go retry =
    if retry >= 10 then failwith "Impossible to create a test directory";
    let directory = Filename.concat root (Fmt.str "%a" fmt (random_string 4)) in
    if Sys.file_exists directory then go (succ retry) else mkdir { directory }
  in
  go 0

let run { directory } { title; fn; _ } =
  let old_stderr = Unix.dup Unix.stderr in
  let new_stderr =
    open_out (Filename.concat directory (Fmt.str "%s.stderr" title))
  in
  Unix.dup2 (Unix.descr_of_out_channel new_stderr) Unix.stderr;
  let finally () =
    flush stderr;
    Unix.dup2 old_stderr Unix.stderr;
    Unix.close old_stderr;
    close_out new_stderr
  in
  Format.eprintf "*** %s ***\n%!" title;
  try Fun.protect ~finally fn
  with exn ->
    let ic = open_in (Filename.concat directory (Fmt.str "%s.stderr" title)) in
    let ln = in_channel_length ic in
    let rs = Bytes.create ln in
    really_input ic rs 0 ln;
    Format.printf "Terminated with: %S\n%!" (Printexc.to_string exn);
    Format.printf "%s\n%!" (Bytes.unsafe_to_string rs);
    exit 1
