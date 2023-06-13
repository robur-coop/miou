open Miou

let () =
  Miou.run @@ fun () ->
  let a = Prm.call_cc (Fun.const ()) in
  let b = Prm.call_cc (fun () -> Prm.await_exn a) in
  Prm.await_all_ign [ a; b ]
