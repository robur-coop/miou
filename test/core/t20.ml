let show () = print_endline "Resource released"

let () =
  Miou.run @@ fun () ->
  let p =
    Miou.call_cc @@ fun () ->
    let _ = Miou.Ownership.own ~finally:show () in
    ()
  in
  Miou.await_exn p
