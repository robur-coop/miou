open Miou

let prgm () =
  Miou.run @@ fun () ->
  let a = Prm.call_cc (fun () -> Unix.sleep 1) in
  let b = Prm.call_cc (fun () -> Unix.sleep 2) in
  Prm.await_all_ign [ a; b ]

let () =
  let t0 = Unix.gettimeofday () in
  prgm ();
  let t1 = Unix.gettimeofday () in
  Format.printf "%f\n%!" (t1 -. t0)
