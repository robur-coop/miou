open Miou

let prgm () =
  Miouu.run @@ fun () ->
  let a = Prm.call (fun () -> Miouu.sleep 10.) in
  Miouu.sleep 0.1;
  Prm.cancel a;
  match Prm.await a with Error Miou.Prm.Cancelled -> () | _ -> failwith "t05"

let () =
  let t0 = Unix.gettimeofday () in
  prgm ();
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 < 10.)
