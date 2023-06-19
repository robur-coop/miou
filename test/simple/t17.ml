open Miou

let print () = print_endline "Resource released"

let () =
  Miou.run @@ fun () ->
  let p =
    Prm.call_cc @@ fun () ->
    let _ = Own.own ~finally:print () in
    raise (Failure "p")
  in
  Prm.await_exn p
