(* NOTE(dinosaure): This code show a basic usage of [Miouu.Cond.t]. *)

let () =
  Miouu.run @@ fun () ->
  let v = ref None in
  let m = Mutex.create () in
  let t = Miouu.Cond.make ~mutex:m () in
  let p0 () =
    Mutex.lock m;
    v := Some ();
    Miouu.Cond.signal t;
    Mutex.unlock m
  in
  let p1 () =
    while Miouu.Cond.wait ~predicate:(fun () -> Option.is_none !v) t do
      ()
    done;
    match !v with Some v -> v | None -> failwith "p1"
  in
  Miou.parallel (function `p0 -> p0 () | `p1 -> p1 ()) [ `p0; `p1 ]
  |> List.iter (function Error exn -> raise exn | Ok () -> ())
