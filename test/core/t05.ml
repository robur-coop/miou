let prgm () =
  Miouu.run @@ fun () ->
  let a = Miou.call (fun () -> Miouu.sleep 10.) in
  Miouu.sleep 0.1;
  Miou.cancel a;
  match Miou.await a with Error Miou.Cancelled -> () | _ -> failwith "t05"

let () =
  let t0 = Unix.gettimeofday () in
  prgm ();
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 < 10.)
