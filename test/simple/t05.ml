open Miou

let () = Printexc.record_backtrace true

let () =
  let a = Unix.gettimeofday () in
  Miou.run (fun () ->
      (* TODO(dinosaure): actually, this test works even with one core because we
         don't run [Unix.sleep 10] yet. We do that only when we [Prm.await]. Of
         course, before [Prm.await], we [Prm.cancel]led our promise. However, keep
         in our mind that if another promise is allocated, we will probably take
         randomly the first one and we are going to occupy our only cpu.

         To improve our test, we probably should use [Prm.call] (which allocates a
         new domain/cpu).

         NOTE(dinosaure): We handle domains now! The [print_endline] ensures that
         we execute our task. It should appears in any cases. *)
      let a = Prm.call (fun () -> print_endline "Launched!"; Unix.sleep 10) in
      Unix.sleep 1;
      (* NOTE(dinosaure): be sure that [Launched!] was printed out! *)
      Prm.cancel a;
      match Prm.await a with Error Miou.Prm.Cancelled -> () | _ -> exit 1);
  let b = Unix.gettimeofday () in
  assert (b -. a < 10.)
