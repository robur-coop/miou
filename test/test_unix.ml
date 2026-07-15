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

let test_udp_echo =
  let description = {text|A simple echo with sendto/recvfrom|text} in
  Test.test ~title:"echo" ~description @@ fun () ->
  Miou_unix.run ~domains:0 @@ fun () ->
  let server = Miou_unix.udpv4 () in
  Miou_unix.bind_and_listen server (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  let sockaddr = Unix.getsockname (Miou_unix.to_file_descr server) in
  let payload = "hello, world!" in
  let server_task =
    Miou.async @@ fun () ->
    let buf = Bytes.create 0x100 in
    let len, client_addr = Miou_unix.recvfrom server buf [] in
    let msg = Bytes.sub_string buf 0 len in
    let reply = String.uppercase_ascii msg in
    let sent = Miou_unix.sendto server reply [] client_addr in
    (msg, sent)
  in
  let client_task =
    Miou.async @@ fun () ->
    let client = Miou_unix.udpv4 () in
    let sent = Miou_unix.sendto client payload [] sockaddr in
    let buf = Bytes.create 0x100 in
    let len, _ = Miou_unix.recvfrom client buf [] in
    let reply = Bytes.sub_string buf 0 len in
    Miou_unix.close client; (sent, reply)
  in
  let a, b = Miou.await_exn server_task in
  let x, y = Miou.await_exn client_task in
  Miou_unix.close server;
  Test.check (a = payload);
  Test.check (x = String.length payload);
  Test.check (b = String.length (String.uppercase_ascii payload));
  Test.check (y = String.uppercase_ascii payload)

let test_udp_stream_mismatch =
  let description = {text|TCP/UDP guards|text} in
  Test.test ~title:"udp and tcp" ~description @@ fun () ->
  Miou_unix.run ~domains:0 @@ fun () ->
  let exn fn =
    match fn () with
    | () -> false
    | exception Invalid_argument _ -> true
    | exception _ -> false
  in
  let udp = Miou_unix.udpv4 () in
  let tcp = Miou_unix.tcpv4 () in
  let buf = Bytes.create 0x7ff in
  Test.check (exn @@ fun () -> ignore (Miou_unix.read udp buf));
  Test.check (exn @@ fun () -> Miou_unix.write udp "x");
  Test.check (exn @@ fun () -> ignore (Miou_unix.recvfrom tcp buf []));
  let fn () =
    ignore
      (Miou_unix.sendto tcp "x" []
         (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)))
  in
  Test.check (exn fn);
  Miou_unix.close udp;
  Miou_unix.close tcp

let test_connect_refused =
  let description = {text|A failing connect must signal the writer|text} in
  Test.test ~title:"connect refused" ~description @@ fun () ->
  Miou_unix.run ~domains:0 @@ fun () ->
  let m = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind m (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  let sockaddr = Unix.getsockname m in
  Unix.close m;
  let socket = Miou_unix.tcpv4 () in
  let connect =
    Miou.async @@ fun () ->
    match Miou_unix.connect socket sockaddr with
    | () -> `Connected
    | exception Unix.Unix_error (Unix.ECONNREFUSED, _, _) -> `Refused
  and timeout = Miou.async @@ fun () -> Miou_unix.sleep 2.; `Timeout in
  let jobs = [ connect; timeout ] in
  let result = Miou.await_one jobs in
  List.iter Miou.cancel jobs;
  Miou_unix.close socket;
  Test.check (result = Ok `Refused)

let () =
  let tests =
    [|
       test_eof_on_pipe; test_create_process; test_udp_echo
     ; test_udp_stream_mismatch; test_connect_refused
    |]
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
