open Miou

let prgm () =
  Miou.run @@ fun () ->
  let a = Prm.call_cc (Fun.const 1) in
  let b = Prm.call_cc (Fun.const 2) in
  Prm.await_exn a + Prm.await_exn b

let () = prgm () |> Format.printf "%d\n%!"
