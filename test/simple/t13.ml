open Miou

let prgm () =
  Miou.run @@ fun () ->
  let a = Prm.call_cc (fun () -> Unix.sleepf 0.2) in
  let b = Prm.call_cc (fun () -> Unix.sleepf 0.4) in
  Prm.await_all_ign [ a; b ]

let () =
  let t0 = Unix.gettimeofday () in
  prgm ();
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 >= 0.6)
