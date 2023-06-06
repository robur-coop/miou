open Miou

let () =
  Miou.run (fun () ->
      let a = Prm.call_cc (Fun.const 1) in
      let b = Prm.call_cc (Fun.const 2) in
      Prm.(await_exn a + await_exn b))
  |> Format.printf "%d\n%!"
