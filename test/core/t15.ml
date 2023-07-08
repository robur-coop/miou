let prgm () =
  Miou.run @@ fun () ->
  let p = Miou.call_cc (fun () -> Unix.sleepf 0.2) in
  Miou.yield (); Miou.cancel p

let () =
  let t0 = Unix.gettimeofday () in
  prgm ();
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 >= 0.2)
