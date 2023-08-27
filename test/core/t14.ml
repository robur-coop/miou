(* NOTE(dinosaure): We show up basically what is the behavior of [miou] with
   several compositions of [call]/[call_cc] and [sleep]/[Unix.sleepf]. *)

let () =
  let () =
    Atomic.set Miouc.tick 0;
    Miouc.run @@ fun () ->
    let a = Miou.call_cc (fun () -> Miouc.sleep 1) in
    let b = Miou.call_cc (fun () -> Miouc.sleep 2) in
    Miou.await_all [ a; b ] |> ignore
  in
  assert (Atomic.get Miouc.tick = 2)

let () =
  let () =
    Atomic.set Miouc.tick 0;
    Miouc.run @@ fun () -> ignore (Miou.parallel Miouc.sleep [ 1; 2 ])
  in
  assert (Atomic.get Miouc.tick = 2)
