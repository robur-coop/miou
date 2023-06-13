open Miou

let () =
  let a = Unix.gettimeofday () in
  let v =
    Miou.run @@ fun () ->
    let a = Prm.call (fun () -> Unix.sleepf 0.4; 2) in
    let b = Prm.call (fun () -> Unix.sleepf 0.2; 1) in
    Prm.await_exn a + Prm.await_exn b
  in
  let b = Unix.gettimeofday () in
  assert (b -. a >= 0.2 && b -. a < 0.6);
  Format.printf "%d\n%!" v
