open Miou

let print () = print_endline "Resource released"

let () =
  Miou.run @@ fun () ->
  let p =
    Prm.call_cc @@ fun () ->
    let r = Own.own ~finally:print () in
    Own.transmit r; raise (Failure "p")
  in
  ignore (Prm.await p);
  print_endline "Resource transmitted";
  raise (Failure "dom0")
