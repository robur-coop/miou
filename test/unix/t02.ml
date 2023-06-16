open Miou
open Miouu

let () =
  Miou.run @@ fun () ->
  let a =
    Prm.call_cc @@ fun () ->
    let b = Prm.call_cc (Fun.const ()) in
    ignore (sleep 1.);
    ignore (Prm.await_exn b)
  in
  yield (); Prm.cancel a
