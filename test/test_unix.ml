let test01 =
  let description = {text|Test waiting for EOF|text} in
  Test.test ~title:"test01" ~description @@ fun () ->
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
    status = Unix.WEXITED 0
  in
  let jobs = [ read; timeout; wait ] in
  begin match Miou.await_one jobs with
  | Ok x -> Test.check x
  | Error _ -> Test.check false
  end;
  List.iter Miou.cancel jobs

let test02 =
  let description = {text|Unix.create_process|text} in
  Test.test ~title:"test02" ~description @@ fun () ->
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
    let status = Miou.Computation.await_exn cmp in
    (* NOTE(dinosaure): better to restore the default because we use
       [fork(3am)] then (see [with_watchdog]). *)
    ignore (Miou.sys_signal Sys.sigchld Sys.Signal_default);
    match status with
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

let test03 =
  let description = {text|A simple echo with sendto/recvfrom|text} in
  Test.test ~title:"test03" ~description @@ fun () ->
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

let test04 =
  let description = {text|TCP/UDP guards|text} in
  Test.test ~title:"test04" ~description @@ fun () ->
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

let test05 =
  let description = {text|A failing connect must signal the writer|text} in
  Test.test ~title:"test05" ~description @@ fun () ->
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

(* NOTE(dinosaure): the following tests are regressions for the
   edge-triggered [epoll] backend. Without the fixes, they do not fail: they
   hang forever (sometimes the whole domain is blocked into a [Unix.read]). We
   therefore run them into a child process killed after [timeout] seconds. *)

let with_watchdog ?(timeout = 5.) fn =
  flush_all ();
  match Unix.fork () with
  | 0 ->
      let code =
        match fn () with
        | true -> 0
        | false -> 1
        | exception exn ->
            Format.eprintf "Unexpected exception: %s\n%!"
              (Printexc.to_string exn);
            2
      in
      Unix._exit code
  | pid ->
      let deadline = Unix.gettimeofday () +. timeout in
      let rec go () =
        match Unix.waitpid [ Unix.WNOHANG ] pid with
        | 0, _ when Unix.gettimeofday () < deadline -> Unix.sleepf 0.01; go ()
        | 0, _ ->
            Format.eprintf "Timeout: the child process is killed\n%!";
            Unix.kill pid Sys.sigkill;
            ignore (Unix.waitpid [] pid);
            false
        | _, Unix.WEXITED 0 -> true
        | _, _ -> false
      in
      go ()

let int_of_fd : Unix.file_descr -> int = Obj.magic

let test06 =
  let description = {text|Partial reads on a blocking file-descriptor|text} in
  Test.test ~title:"test06" ~description @@ fun () ->
  Test.check @@ with_watchdog
  @@ fun () ->
  Miou_unix.run ~domains:0 @@ fun () ->
  let a, b = Unix.socketpair ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let a = Miou_unix.of_file_descr ~non_blocking:false a in
  let writer =
    Miou.async @@ fun () ->
    Miou_unix.sleep 0.1;
    ignore (Unix.write_substring b "0123456789" 0 10)
  in
  let buf = Bytes.create 0x100 in
  let n1 = Miou_unix.read a ~len:4 buf in
  (* NOTE(dinosaure): 6 bytes remain but no new edge will come. *)
  let n2 = Miou_unix.read a buf in
  Miou.await_exn writer;
  Miou_unix.close a;
  Unix.close b;
  n1 = 4 && n2 = 6 && Bytes.sub_string buf 0 6 = "456789"

let test07 =
  let description =
    {text|A stale readiness on a blocking file-descriptor|text}
  in
  Test.test ~title:"test07" ~description @@ fun () ->
  Test.check @@ with_watchdog
  @@ fun () ->
  Miou_unix.run ~domains:0 @@ fun () ->
  let a, b = Unix.socketpair ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let a' = Miou_unix.of_file_descr ~non_blocking:false a in
  let writer =
    Miou.async @@ fun () ->
    Miou_unix.sleep 0.05;
    ignore (Unix.write_substring b "abcd" 0 4)
  in
  let buf = Bytes.create 0x100 in
  let n1 = Miou_unix.read a' buf in
  Miou.await_exn writer;
  (* NOTE(dinosaure): an edge arrives while nobody waits for [a] *)
  ignore (Unix.write_substring b "efgh" 0 4);
  Miou_unix.sleep 0.05;
  (* NOTE(dinosaure): and these bytes are consumed elsewhere. *)
  let n2 = Unix.read a buf 0 (Bytes.length buf) in
  let writer =
    Miou.async @@ fun () ->
    Miou_unix.sleep 0.1;
    ignore (Unix.write_substring b "ijkl" 0 4)
  in
  (* NOTE(dinosaure): we must wait for [writer] and not block the domain into
     [Unix.read] because of a stale readiness. *)
  let n3 = Miou_unix.read a' buf in
  Miou.await_exn writer;
  Miou_unix.close a';
  Unix.close b;
  n1 = 4 && n2 = 4 && n3 = 4 && Bytes.sub_string buf 0 4 = "ijkl"

let test08 =
  let description =
    {text|Re-use a file-descriptor released by a cancelled owner|text}
  in
  Test.test ~title:"test08" ~description @@ fun () ->
  Test.check @@ with_watchdog
  @@ fun () ->
  Miou_unix.run ~domains:0 @@ fun () ->
  let a, b = Unix.socketpair ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let owner =
    Miou.async @@ fun () ->
    let a = Miou_unix.Ownership.of_file_descr a in
    let buf = Bytes.create 0x100 in
    ignore (Miou_unix.Ownership.read a buf)
  in
  Miou_unix.sleep 0.05;
  (* NOTE(dinosaure): [a] is closed by the finaliser of its resource. *)
  Miou.cancel owner;
  Unix.close b;
  let c, d = Unix.socketpair ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let reused = int_of_fd c = int_of_fd a in
  let c = Miou_unix.of_file_descr c in
  let writer =
    Miou.async @@ fun () ->
    Miou_unix.sleep 0.1;
    ignore (Unix.write_substring d "hello" 0 5)
  in
  let buf = Bytes.create 0x100 in
  let n = Miou_unix.read c buf in
  Miou.await_exn writer;
  Miou_unix.close c;
  Unix.close d;
  reused && n = 5 && Bytes.sub_string buf 0 5 = "hello"

let test09 =
  let description =
    {text|Re-use a file-descriptor closed by another domain|text}
  in
  Test.test ~title:"test09" ~description @@ fun () ->
  Test.check @@ with_watchdog
  @@ fun () ->
  Miou_unix.run ~domains:1 @@ fun () ->
  let a, b = Unix.socketpair ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let a' = Miou_unix.of_file_descr a in
  let writer =
    Miou.async @@ fun () ->
    Miou_unix.sleep 0.05;
    ignore (Unix.write_substring b "x" 0 1)
  in
  let buf = Bytes.create 0x100 in
  let n1 = Miou_unix.read a' buf in
  Miou.await_exn writer;
  (* NOTE(dinosaure): [a] is watched by this domain but closed by another one. *)
  Miou.await_exn (Miou.call @@ fun () -> Miou_unix.close a');
  Unix.close b;
  let c, d = Unix.socketpair ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let reused = int_of_fd c = int_of_fd a in
  let c = Miou_unix.of_file_descr c in
  let writer =
    Miou.async @@ fun () ->
    Miou_unix.sleep 0.1;
    ignore (Unix.write_substring d "hello" 0 5)
  in
  let n2 = Miou_unix.read c buf in
  Miou.await_exn writer;
  Miou_unix.close c;
  Unix.close d;
  reused && n1 = 1 && n2 = 5

let test10 =
  let description = {text|connect must wait for the connection|text} in
  Test.test ~title:"test10" ~description @@ fun () ->
  Test.check @@ with_watchdog
  @@ fun () ->
  Miou_unix.run ~domains:0 @@ fun () ->
  let listener = Miou_unix.tcpv4 () in
  Miou_unix.bind_and_listen ~backlog:0 listener
    (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  let sockaddr = Unix.getsockname (Miou_unix.to_file_descr listener) in
  let fillers =
    List.init 2 @@ fun _ ->
    let fd = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.set_nonblock fd;
    (try Unix.connect fd sockaddr with Unix.Unix_error _ -> ());
    fd
  in
  (* NOTE(dinosaure): the internal queue of [listener] should be full now. *)
  Miou_unix.sleep 0.05;
  (* NOTE(dinosaure): set a readiness (writable) on [a] while nobody writes. *)
  let a, b = Unix.socketpair ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let a' = Miou_unix.of_file_descr a in
  let writer =
    Miou.async @@ fun () ->
    Miou_unix.sleep 0.05;
    ignore (Unix.write_substring b "x" 0 1)
  in
  ignore (Miou_unix.read a' (Bytes.create 0x100));
  Miou.await_exn writer;
  (* NOTE(dinosaure): the user closes the file-descriptor by himself. *)
  Unix.close (Miou_unix.to_file_descr a');
  Unix.close b;
  let socket = Miou_unix.tcpv4 () in
  let reused = int_of_fd (Miou_unix.to_file_descr socket) = int_of_fd a in
  (* NOTE(dinosaure): if [reused], our [epoll(7)] backend considers [a] as
     writable due to our old file descriptor [a] even if we created a new file
     description [socket] (which has the same value as [a]). *)
  let is_connected socket =
    match Unix.getpeername (Miou_unix.to_file_descr socket) with
    | _ -> `Connected
    | exception Unix.Unix_error (Unix.ENOTCONN, _, _) -> `Not_connected
  in
  let connect =
    Miou.async @@ fun () ->
    (* NOTE(dinosaure): how our [Miou_unix.connect] reacts if, on the
       [epoll(7)] backend, our [ready] table says the [socket] is writable? *)
    match Miou_unix.connect socket sockaddr with
    | () -> is_connected socket
    | exception Unix.Unix_error (err, _, _) -> `Error err
  and timeout = Miou.async @@ fun () -> Miou_unix.sleep 0.5; `Timeout in
  let jobs = [ connect; timeout ] in
  let result = Miou.await_one jobs in
  List.iter Miou.cancel jobs;
  Miou_unix.close socket;
  List.iter Unix.close fillers;
  Miou_unix.close listener;
  let pp ppf = function
    | Ok `Connected -> Fmt.string ppf "connected"
    | Ok `Not_connected -> Fmt.string ppf "not connected"
    | Ok (`Error err) -> Fmt.pf ppf "error: %s" (Unix.error_message err)
    | Ok `Timeout -> Fmt.string ppf "timeout"
    | Error exn -> Fmt.pf ppf "exception: %s" (Printexc.to_string exn)
  in
  Format.eprintf "test10: fd reused: %b, connect: %a\n%!" reused pp result;
  let is_ok = function
    | Ok (`Connected | `Timeout | `Error _) -> true
    | Ok `Not_connected | Error _ -> false
  in
  (* NOTE(dinosaure): if we reused the same file descriptor value, we should
     have an "acceptable" value. [`Not_connected] appears when
     [Miou_unix.connect] is resumed by Miou whereas the socket is **not**
     connected. *)
  (not reused) || is_ok result

let () =
  let tests =
    [|
       test01; test02; test03; test04; test05; test06; test07; test08; test09
     ; test10
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
