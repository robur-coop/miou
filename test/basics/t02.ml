(* NOTE(dinosaure): This code show a basic usage of [Miouu.Cond.t]. *)

let () =
  Miouu.run @@ fun () ->
  let v = Atomic.make None in
  let t = Miouu.Cond.make () in
  let p0 = Miou.call @@ fun () -> Atomic.set v (Some ()); Miouu.Cond.signal t in
  let p1 =
    Miou.call @@ fun () ->
    Miouu.Cond.wait ~fn:ignore t;
    match Atomic.get v with Some v -> v | None -> failwith "p1"
  in
  Miou.await_all [ p0; p1 ]
  |> List.iter (function Error exn -> raise exn | Ok () -> ())
