open Miou

let () =
  let a = Unix.gettimeofday () in
  let _ =
    Miou.run @@ fun () ->
    let a = Prm.call (fun () -> Unix.sleep 1; 1) in
    let b = Prm.call (fun () -> Unix.sleep 1; 1) in
    Prm.await_exn a + Prm.await_exn b
  in
  let b = Unix.gettimeofday () in
  assert (b -. a >= 1. && b -. a < 2.)
(* XXX(dinosaure): this is ensure that we execute in parallel [a] and [b]. *)
