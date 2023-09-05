(* NOTE(dinosaure): We show up that [yield] tries to run pending tasks. In our
   case, the only one is [p] and we should sleep 1s. Even if after we cancel it,
   we should spend 1s. This code works only with [Miou.call_cc] & [Unix.sleepf].
*)

let prgm () =
  Miouc.run @@ fun () ->
  let p = Miou.call_cc (fun () -> Miouc.sleep 1) in
  Miou.yield (); Miou.cancel p

let () =
  let () = prgm () in
  assert (Atomic.get Miouc.tick = 1)
