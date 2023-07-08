
let prgm () =
  Miou.run @@ fun () ->
  let a = Miou.call_cc (Fun.const 1) in
  let b = Miou.call_cc (Fun.const 2) in
  Miou.await_exn a + Miou.await_exn b

let () = prgm () |> Format.printf "%d\n%!"
