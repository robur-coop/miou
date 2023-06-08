open Miou

let () =
  Miou.run (fun () ->
      let th0, wk = Var.make () in
      let th1 = Prm.call (fun () -> Unix.sleep 1; Var.resolve wk ()) in
      Prm.await_all_exn [ th0; th1 ])
  |> function
  | Ok () -> ()
  | Error exn -> raise exn
