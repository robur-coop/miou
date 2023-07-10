(* NOTE(dinosaure): This test must fail with [Not_a_child]. It's invalid with
   [miou] to wait a promise that the current task did not launch. *)

let () =
  Miou.run @@ fun () ->
  let a = Miou.call_cc (Fun.const ()) in
  let b = Miou.call_cc (fun () -> Miou.await_exn a) in
  Miou.await_exn a; Miou.await_exn b
