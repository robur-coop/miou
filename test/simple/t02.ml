open Miou

let () =
  Miou.run (fun () ->
      let one = Prm.call_cc (Fun.const 1) in
      let two = Prm.call_cc (Fun.const 2) in
      Prm.await_first [ one; two ])
  |> function
  | Ok n -> Format.printf "%d\n%!" n
  | Error exn -> raise exn
