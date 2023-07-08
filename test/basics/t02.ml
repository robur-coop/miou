open Miou

let () =
  Miouu.run @@ fun () ->
  let v = Atomic.make None in
  let t = Miouu.Cond.make () in
  let p0 = Prm.call @@ fun () -> Atomic.set v (Some ()); Miouu.Cond.signal t in
  let p1 =
    Prm.call @@ fun () ->
    Miouu.Cond.wait ~fn:ignore t;
    match Atomic.get v with Some v -> v | None -> failwith "p1"
  in
  Prm.await_all [ p0; p1 ]
  |> List.iter (function Error exn -> raise exn | Ok () -> ())
