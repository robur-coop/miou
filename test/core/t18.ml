open Miou

let () =
  Miou.run ~domains:0 @@ fun () ->
  let p = Prm.call (Fun.const ()) in
  Prm.await_exn p
