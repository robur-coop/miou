(* NOTE(dinosaure): We show up that if the parent fail, children are cancelled
   too. In that case, we get the [Still_has_children] exception because we did
   not await our sleeper. [miou] will then cancel our sleeper and raise our
   exception. We check that we did not spend 10s and the program should
   terminate with [Fatal error: exception Miou.Still_has_children]. *)

let prgm () =
  Miouc.run @@ fun () -> ignore (Miou.call (fun () -> Miouc.sleep 10))

let () =
  let rs = try Ok (prgm ()) with exn -> Error exn in
  match rs with
  | Ok () -> failwith "t09"
  | Error exn ->
      assert (Atomic.get Miouc.tick = 0);
      raise exn
