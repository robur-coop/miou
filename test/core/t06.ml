
let sleep () = Unix.sleepf 1.

let prgm () =
  Miou.run @@ fun () ->
  let a = Miou.call sleep in
  let b = Miou.call sleep in
  Miou.await_exn a; Miou.await_exn b

let () =
  let t0 = Unix.gettimeofday () in
  prgm ();
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 < 2.)
