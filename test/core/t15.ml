(* NOTE(dinosaure): We show up that [yield] tries to run pending tasks. In our
   case, the only one is [p] and we should sleep 1s. Even if after we cancel it,
   we should spend 1s. This code works only with [Miou.call_cc] & [Unix.sleepf].
*)

let prgm () =
  Miou.run @@ fun () ->
  let p = Miou.call_cc (fun () -> Unix.sleepf 1.) in
  Miou.yield (); Miou.cancel p

let () =
  let t0 = Unix.gettimeofday () in
  prgm ();
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 >= 1.)
