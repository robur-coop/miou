open Miou

let () =
  Miou.run @@ fun () ->
  let p0 = Prm.call_cc @@ fun () -> Prm.call_cc (Fun.const ()) in
  let p1 = Prm.await_exn p0 in
  Prm.await_exn p1
