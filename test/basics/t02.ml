(* NOTE(dinosaure): This code show a basic usage of [Miou_unix.Cond.t]. *)

let () =
  Miou_unix.run ~domains:2 @@ fun () ->
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
    Mutex.lock m;
    while Option.is_none !v do
      Mutex.unlock m; Miou_unix.Cond.wait t; Mutex.lock m
    done;
    let v = !v in
    Mutex.unlock m;
    match v with Some v -> v | None -> failwith "p1"
  in
  Miou.parallel (function `p0 -> p0 () | `p1 -> p1 ()) [ `p0; `p1 ]
  |> List.iter (function Error exn -> raise exn | Ok () -> ())
