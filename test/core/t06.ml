(* NOTE(dinosaure): We show up the parallelism! This code should spend less than
   2s. [a] and [b] uses [Unix.sleepf] which suspends the task and it **does
   not** notify [miou]. Even if we use [Unix.sleepf], because [a] and [b] are
   in parallel, we should consume only 1s to terminate this program. *)

let sleep () = Unix.sleepf 1.
let prgm () = Miou.run @@ fun () -> ignore (Miou.parallel sleep [ (); () ])

let () =
  let t0 = Clock.now () in
  prgm ();
  let t1 = Clock.now () in
  assert (t1 -. t0 < 2.)
