(* NOTE(dinosaure): We show up the abnormal termination of task here. When [p]
   raises an exception, we cancel properly children (in our case, [child_of_p])
   and return the exception. In the case of an abnormal termination, we must
   terminate as soon as we can - and we check that we did not spend 1s.
   Otherwise, we check that we spent 1s in the normal situation. *)

let () = Random.self_init ()

exception Basic_failure

let prgm () =
  Atomic.set Miouc.tick 0;
  Miouc.run @@ fun () ->
  let p =
    Miou.call_cc @@ fun () ->
    let child_of_p = Miou.call_cc (fun () -> Miouc.sleep 1) in
    if Random.bool () then raise Basic_failure;
    Miou.await_exn child_of_p
  in
  Miou.await p

let rec until_its_ok () =
  match prgm () with
  | Ok () -> assert (Atomic.get Miouc.tick = 1)
  | Error Basic_failure -> until_its_ok ()
  | _ -> failwith "t04"

let rec until_its_abnormal () =
  match prgm () with
  | Error Basic_failure -> assert (Atomic.get Miouc.tick = 0)
  | Ok () -> until_its_abnormal ()
  | Error _ -> failwith "t04"

let () = until_its_ok (); until_its_abnormal ()
