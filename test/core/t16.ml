(* NOTE(dinosaure): we can await multiple times a task. *)

let () =
  Miou.run @@ fun () ->
  let p = Miou.call (fun () -> Miouu.sleep 0.1) in
  Miou.await_exn p; Miou.await_exn p
