let print () = print_endline "Resource released"

let () =
  Miou.run @@ fun () ->
  let p =
    Miou.call_cc @@ fun () ->
    let t = Miou.Ownership.own ~finally:print () in
    Miou.Ownership.disown t
  in
  Miou.await_exn p
