let () =
  Miou.run @@ fun () ->
  let p = Miou.call (fun () -> Unix.sleepf 0.1) in
  Miou.await_exn p; Miou.await_exn p
