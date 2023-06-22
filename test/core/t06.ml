open Miou

let sleep () = Unix.sleepf 0.1

let prgm () =
  Miou.run @@ fun () ->
  let a = Prm.call sleep in
  let b = Prm.call sleep in
  Prm.await_exn a; Prm.await_exn b

let () =
  let t0 = Unix.gettimeofday () in
  prgm ();
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 < 0.2)
