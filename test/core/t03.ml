(* NOTE(dinosaure): this test must always fail with [Still_has_children]. *)

let () = Miou.run @@ fun () -> ignore (Miou.call_cc (Fun.const ()))
