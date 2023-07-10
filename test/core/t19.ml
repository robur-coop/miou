(* NOTE(dinosaure): We show up the ownership behavior and the abnormal
   situation. [miou] releases all resources in that case. *)

let print () = print_endline "Resource released"

let () =
  Miou.run @@ fun () ->
  let p =
    Miou.call_cc @@ fun () ->
    let _ = Miou.Ownership.own ~finally:print () in
    raise (Failure "p")
  in
  Miou.await_exn p
