
let prgm () =
  Miouu.run @@ fun () -> ignore (Miou.call (fun () -> Miouu.sleep 10.))

let () =
  let t0 = Unix.gettimeofday () in
  let rs = try Ok (prgm ()) with exn -> Error exn in
  let t1 = Unix.gettimeofday () in
  match rs with
  | Ok () -> failwith "t09"
  | Error exn ->
      assert (t1 -. t0 < 10.);
      raise exn
