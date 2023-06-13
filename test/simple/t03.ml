open Miou

let () = Miou.run (fun () -> ignore (Prm.call_cc (Fun.const 1)))
