
let prgm () =
  Miou.run @@ fun () ->
  let p = Miou.call (Fun.const ()) in
  Miou.await_exn p; Miou.cancel p; Miou.await_exn p

let () =
  for _ = 0 to 100 do
    prgm ()
  done
