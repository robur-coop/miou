
let () =
  Miou.run ~domains:0 @@ fun () ->
  let p = Miou.call (Fun.const ()) in
  Miou.await_exn p
