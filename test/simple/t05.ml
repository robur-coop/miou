open Miou

let () = Printexc.record_backtrace true

let () =
  let a = Unix.gettimeofday () in
  Miou.run (fun () ->
      let a = Prm.call (fun () -> print_endline "Launched!"; Unix.sleepf 10.) in
      Unix.sleepf 0.1;
      Prm.cancel a;
      match Prm.await a with Error Miou.Prm.Cancelled -> () | _ -> exit 1);
  let b = Unix.gettimeofday () in
  assert (b -. a < 10.)
