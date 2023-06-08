open Miou

let () =
  Miou.run @@ fun () ->
  let _ = Prm.call_cc (Fun.const 1) in
  Prm.yield ()
