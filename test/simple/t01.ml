open Miou

let () =
  Miou.run (fun () ->
      let a = Promise.call_cc (Fun.const 1) in
      let b = Promise.call_cc (Fun.const 2) in
      Promise.(await_exn a + await_exn b))
  |> Format.printf "%d\n%!"
