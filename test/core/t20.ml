open Miou

let show () = print_endline "Resource released"

let () =
  Miou.run @@ fun () ->
  let p =
    Prm.call_cc @@ fun () ->
    let _ = Own.own ~finally:show () in
    ()
  in
  Prm.await_exn p
