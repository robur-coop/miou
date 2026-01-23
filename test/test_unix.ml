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
  (* NOTE(dinosaure): [non_blocking] is mandatory for Windows. *)
  let out = Miou_unix.of_file_descr ~non_blocking:false out in
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
     Windows | true   | false     | exn    |

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

let () =
  let tests = [| test_eof_on_pipe |] in
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
