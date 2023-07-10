(* NOTE(dinosaure): We show up that if the parent fail, children are cancelled
   too. In that case, we get the [Still_has_children] exception because we did
   not await our sleeper. [miou] will then cancel our sleeper and raise our
   exception. We check that we did not spend 10s and the program should
   terminate with [Fatal error: exception Miou.Still_has_children]. *)

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
