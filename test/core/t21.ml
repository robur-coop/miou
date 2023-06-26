open Miou

let print () = print_endline "Resource released"

let () =
  Miou.run @@ fun () ->
  let p =
    Prm.call_cc @@ fun () ->
    let t = Own.own ~finally:print () in
    Own.disown t
  in
  Prm.await_exn p
