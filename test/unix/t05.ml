open Miou

let () =
  Miouu.run @@ fun () ->
  let a =
    Prm.call @@ fun () ->
    let b = Prm.call_cc (Fun.const ()) in
    Miouu.sleep 2.; Prm.await_exn b
  in
  (* XXX(dinosaure): we give a chance for [a] to spawn [b]. *)
  Miouu.sleep 0.01; Prm.cancel a
