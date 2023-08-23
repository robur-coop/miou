(* NOTE(dinosaure): we can await multiple times a task. *)

let prgm () =
  Miou.run @@ fun () ->
  let p = Miou.call (fun () -> Unix.sleepf 0.1) in
  Miou.await_exn p; Miou.await_exn p

let () =
  let t0 = Clock.now () in
  prgm ();
  let t1 = Clock.now () in
  assert (t1 -. t0 < 0.2)
