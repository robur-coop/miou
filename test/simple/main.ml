open Miou

let () = Printexc.record_backtrace true

let fiber_a () =
  Unix.sleep 1;
  1

let fiber_b () =
  Unix.sleep 2;
  2

let () =
  Miou.run (fun () ->
      let a = Promise.call_cc fiber_a in
      let b = Promise.call_cc fiber_b in
      Promise.(await_exn a + await_exn b))
  |> Format.printf "%d\n%!"
