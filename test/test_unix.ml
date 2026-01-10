let test_eof =
  let description = {text|Test waiting for EOF|text} in
  Test.test ~title:"EOF" ~description @@ fun () ->
  Miou_unix.run ~domains:0 @@ fun () ->
  let cmd = "true" in
  let out, cmd_stdout = Unix.pipe ~cloexec:true () in
  Unix.clear_close_on_exec cmd_stdout;
  let _pid =
    Unix.(create_process cmd [| cmd; "/dev/null" |]
            stdin cmd_stdout stderr)
  in
  Unix.close cmd_stdout;
  let out = Miou_unix.of_file_descr out in
  let buf = Bytes.create 100 in
  let read =
    Miou.async @@ fun () ->
    match Miou_unix.read out buf with
    | 0 -> true
    | _ -> Miou.Logs.err (fun m -> m "unexpected read"); false
  and timeout = Miou.async @@ fun () ->
    Miou_unix.sleep 1.;
    Miou.Logs.err (fun m -> m "timeout");
    false
  in
  let jobs = [ read; timeout ] in
  begin match Miou.await_one jobs with
  | Ok x -> Test.check x
  | Error _ -> Miou.Logs.err (fun m -> m "unexpected exception"); assert false
  end;
  List.iter Miou.cancel jobs

let () =
  let tests =
    [| test_eof |]
  in
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
