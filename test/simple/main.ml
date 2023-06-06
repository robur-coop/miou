open Miou

let () = Printexc.record_backtrace true

let () =
  Miou.run (fun () ->
      Format.eprintf ">>> make new task a\n%!";
      let a =
        Promise.call_cc (fun () ->
            Format.printf ">>> sleep 1\n%!";
            1)
      in
      Format.eprintf ">>> make new task b\n%!";
      let b =
        Promise.call_cc (fun () ->
            Format.printf ">>> sleep 2\n%!";
            3)
      in
      Promise.(await_exn a + await_exn b))
  |> Format.printf "%d\n%!"
