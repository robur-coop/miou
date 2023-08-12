(* NOTE(dinosaure): We show up the cancellation mechanism here. We have a task
   which should sleep 10s but, after 1s, we cancel it. The [sleep 1.] gives a
   chance for a domain to run [a]. We check that the result of [a] must be
   [Miou.Cancelled] and we check that we did not spend 10s. *)

let prgm () =
  Miouu.run @@ fun () ->
  let a = Miou.call (fun () -> Miouu.sleep 10.) in
  Miouu.sleep 1.;
  Miou.cancel a;
  match Miou.await a with Error Miou.Cancelled -> () | _ -> failwith "t05"

let () =
  let t0 = Clock.now () in
  prgm ();
  let t1 = Clock.now () in
  assert (t1 -. t0 < 10.)
