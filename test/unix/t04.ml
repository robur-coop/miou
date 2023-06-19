open Miou

let () =
  Miouu.run @@ fun () ->
  let a = Prm.call (fun () -> Miouu.sleep 10.) in
  Prm.cancel a;
  match Prm.await a with Error Prm.Cancelled -> () | _ -> exit 1
