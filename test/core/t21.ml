(* NOTE(dinosaure): This code show up a valid usage of the [Ownership] module,
   we can [own] but we must [disown] then. In that situation, [miou] does not
   execute the finaliser. *)

let print () = print_endline "Resource released"

let () =
  Miou.run @@ fun () ->
  let p =
    Miou.call_cc @@ fun () ->
    let t = Miou.Ownership.own ~finally:print () in
    Miou.Ownership.disown t
  in
  Miou.await_exn p
