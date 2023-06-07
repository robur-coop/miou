open Miou

(* XXX(dinosaure): check if we verify that we await only our children.
   This code should always raise an exception. *)
let () =
  Miou.run @@ fun () ->
  let a = Prm.call_cc (Fun.const ()) in
  let b = Prm.call_cc (fun () -> Prm.await_exn a) in
  Prm.await_first_exn [ a; b ]
