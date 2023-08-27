(* NOTE(dinosaure): This code show a basic usage of [Miou_unix.Cond.t]. *)

let () =
  Miou_unix.run @@ fun () ->
  let v = ref None in
  let m = Mutex.create () in
  let t = Miou_unix.Cond.make ~mutex:m () in
  let p0 () =
    Mutex.lock m;
    v := Some ();
    Miou_unix.Cond.signal t;
    Mutex.unlock m
  in
  let p1 () =
    while Miou_unix.Cond.wait ~predicate:(fun () -> Option.is_none !v) t do
      ()
    done;
    match !v with Some v -> v | None -> failwith "p1"
  in
  Miou.parallel (function `p0 -> p0 () | `p1 -> p1 ()) [ `p0; `p1 ]
  |> List.iter (function Error exn -> raise exn | Ok () -> ())
