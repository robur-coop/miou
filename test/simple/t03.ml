open Miou

(* XXX(dinosaure): this code always fails! *)
let () = Miou.run (fun () -> ignore (Promise.call_cc (Fun.const 1)))
