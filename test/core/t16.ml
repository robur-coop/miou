open Miou

let () =
  Miou.run @@ fun () ->
  let p = Prm.call (fun () -> Miouu.sleep 0.1) in
  Prm.await_exn p; Prm.await_exn p
