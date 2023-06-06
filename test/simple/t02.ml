open Miou

(* XXX(dinosaure): this code prints out [1] or [2]. *)
let () =
  Miou.run (fun () ->
      let one = Promise.call_cc (Fun.const 1) in
      let two = Promise.call_cc (Fun.const 2) in
      Promise.await_first [ one; two ])
  |> function
  | Ok n -> Format.printf "%d\n%!" n
  | Error exn -> raise exn
