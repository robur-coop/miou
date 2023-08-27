(* NOTE(dinosaure): We show up the parallelism! This code should spend less than
   2s. [a] and [b] uses [Unix.sleepf] which suspends the task and it **does
   not** notify [miou]. Even if we use [Unix.sleepf], because [a] and [b] are
   in parallel, we should consume only 1s to terminate this program. *)

let sleep () = Miouc.sleep 1
let prgm () = Miouc.run @@ fun () -> ignore (Miou.parallel sleep [ (); () ])

let () =
  let () = prgm () in
  assert (Atomic.get Miouc.tick = 1)
