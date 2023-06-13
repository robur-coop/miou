open Miou

let () =
  Miou.run @@ fun () ->
  let a =
    Prm.call (fun () -> Unix.sleepf 0.2; yield (); print_endline "Terminated")
  in
  Prm.cancel a;
  Unix.sleepf 0.4;
  match Prm.await a with
  | Error Miou.Prm.Cancelled -> print_endline "Terminated"
  | _ -> exit 1
