(* NOTE(dinosaure): We show up that the user **must** release its resources
   before the end of the task. However, in that situation, [miou] also release
   them. *)

let show () = print_endline "Resource released"

let () =
  Miou.run @@ fun () ->
  let p =
    Miou.call_cc @@ fun () ->
    let _ = Miou.Ownership.own ~finally:show () in
    ()
  in
  Miou.await_exn p
