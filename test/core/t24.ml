(* NOTE(dinosaure): In this code, [p0] must wait [p1] even if it returns the
   promise. This test always fail with [Still_has_children]. *)

let () =
  Miou.run @@ fun () ->
  let p0 = Miou.call_cc @@ fun () -> Miou.call_cc (Fun.const ()) in
  let p1 = Miou.await_exn p0 in
  Miou.await_exn p1
