let system =
  let ic = Unix.open_process_in "uname -s" in
  let system = input_line ic in
  ignore (Unix.close_process_in ic);
  system

let test_eof_on_pipe =
  let description = {text|Test waiting for EOF|text} in
  Test.test ~title:"eof on pipe" ~description @@ fun () ->
  Miou_unix.run ~domains:0 @@ fun () ->
  let cmd = "true" in
  let out, cmd_stdout = Unix.pipe ~cloexec:true () in
  Unix.clear_close_on_exec cmd_stdout;
  let pid = Unix.(create_process cmd [| cmd |] stdin cmd_stdout stderr) in
  Unix.close cmd_stdout;
  let out = Miou_unix.of_file_descr out in
  let buf = Bytes.create 1 in
  let read = Miou.async @@ fun () -> Miou_unix.read out buf = 0
  and timeout = Miou.async @@ fun () -> Miou_unix.sleep 1.; false
  and wait =
    Miou.async @@ fun () ->
    let _, status = Unix.waitpid [] pid in
    if not (status = Unix.WEXITED 0 && system = "Darwin") then
      failwith "Unexpected case";
    true
  in
  (* NOTE(dinosaure): only on MacOS, it seems that the kernel does not
     immediately close our pipe. So we fallback to the [timeout] case which is
     not expected. We have a special case for MacOS where we returns [true] if
     our process ends successfully.

             | [read] | [timeout] | [wait] |
     MacOS   | hang   | false     | true   |
     FreeBSD | true   | false     | exn    |
     Linux   | true   | false     | exn    |

     The goal here is to prevent the [timeout] case by something else:
     - for Linux/FreeBSD, by the fact that we have successfully received the EOF
       signal from our pipe
     - for MacOS, by the fact that our programme ended successfully *)
  let jobs = [ read; timeout; wait ] in
  begin match Miou.await_one jobs with
  | Ok x -> Test.check x
  | Error _ -> Test.check false
  end;
  List.iter Miou.cancel jobs

let test_create_process =
  let description = {text|Unix.create_process|text} in
  Test.test ~title:"create process" ~description @@ fun () ->
  let buf = Buffer.create 0x7ff in
  let prgm () =
    Miou_unix.run ~domains:0 @@ fun () ->
    let pid =
      Unix.create_process "sleep" [| "sleep"; "2" |] Unix.stdin Unix.stdout
        Unix.stderr
    in
    Buffer.add_string buf "sleep launched\n";
    let cmp = Miou.Computation.create () in
    let rec fn _sigchld =
      match Unix.waitpid [ WNOHANG ] pid with
      | 0, _ -> ignore (Miou.sys_signal Sys.sigchld (Sys.Signal_handle fn))
      | pid', status ->
          assert (pid = pid');
          assert (Miou.Computation.try_return cmp status)
    in
    ignore (Miou.sys_signal Sys.sigchld (Sys.Signal_handle fn));
    Buffer.add_string buf "signal handler installed\n";
    match Miou.Computation.await_exn cmp with
    | Unix.WEXITED n -> Buffer.add_string buf (Fmt.str "WEXITED(%d)\n%!" n)
    | Unix.WSIGNALED n -> Buffer.add_string buf (Fmt.str "WSIGNALED(%d)\n%!" n)
    | Unix.WSTOPPED n -> Buffer.add_string buf (Fmt.str "WSIGNALED(%d)\n%!" n)
  in
  match Sys.os_type with
  | "Win32" -> ()
  | _ ->
      prgm ();
      let serialized = Buffer.contents buf in
      let expected = "sleep launched\nsignal handler installed\nWEXITED(0)\n" in
      Test.check (serialized = expected)

let () =
  let tests = [| test_eof_on_pipe; test_create_process |] in
  let ({ Test.directory } as runner) =
    Test.runner (Filename.concat (Sys.getcwd ()) "_tests")
  in
  let run test =
    Format.printf "%s: %!" test.Test.title;
    Test.run runner test;
    Format.printf "ok\n%!"
  in
  Format.printf "Run tests into %s\n%!" directory;
  Array.iter run tests
