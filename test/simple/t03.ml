open Miou

(* XXX(dinosaure): this code always fails! *)
let () = Miou.run (fun () -> ignore (Prm.call_cc (Fun.const 1)))
