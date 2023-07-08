
let () =
  Miou.run @@ fun () ->
  let p0 = Miou.call_cc @@ fun () -> Miou.call_cc (Fun.const ()) in
  let p1 = Miou.await_exn p0 in
  Miou.await_exn p1
