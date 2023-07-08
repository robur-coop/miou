
let () =
  Miou.run @@ fun () ->
  let a = Miou.call_cc (Fun.const ()) in
  let b = Miou.call_cc (fun () -> Miou.await_exn a) in
  Miou.await_exn a; Miou.await_exn b
