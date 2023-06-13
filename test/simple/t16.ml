open Miou

let prgm () =
  Miou.run @@ fun () ->
  let a = Prm.call_cc (fun () -> Unix.sleepf 0.5) in
  yield (); Prm.cancel a

let () =
  let t0 = Unix.gettimeofday () in
  prgm ();
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 >= 0.5)
