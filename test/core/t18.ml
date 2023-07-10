(* NOTE(dinosaure): This test always fails with [No_domain_available]. *)

let () =
  Miou.run ~domains:0 @@ fun () ->
  let p = Miou.call (Fun.const ()) in
  Miou.await_exn p
