open Miou

let () =
  Miou.run @@ fun () ->
  let p = Prm.call (fun () -> Unix.sleepf 0.1) in
  Prm.await_exn p
