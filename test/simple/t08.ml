open Miou

let () =
  let a = Unix.gettimeofday () in
  let v =
    Miou.run @@ fun () ->
    let a = Prm.call (fun () -> Unix.sleep 2; 2) in
    let b = Prm.call (fun () -> Unix.sleep 1; 1) in
    Prm.await_exn a + Prm.await_exn b
  in
  let b = Unix.gettimeofday () in
  assert (b -. a >= 1. && b -. a < 3.);
  Format.printf "%d\n%!" v
