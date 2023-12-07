let prgm () =
  Miouc.run @@ fun () ->
  let a = Miou.call_cc (fun () -> Miouc.sleep 4) in
  let b = Miou.call_cc (fun () -> Miouc.sleep 6) in
  Miou.await_all [ a; b ] |> ignore

let () =
  let () = prgm () in
  assert (Atomic.get Miouc.tick = 6)
