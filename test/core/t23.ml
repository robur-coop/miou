(* NOTE(dinosaure): We show up the basic [with_timeout] function. *)

exception Timeout

let with_timeout s fn =
  let p0 = Miou.call_cc fn in
  let p1 = Miou.call_cc @@ fun () -> Miouc.sleep s; raise Timeout in
  Miou.await_first [ p0; p1 ]

let () =
  match Miouc.run @@ fun () -> with_timeout 10 (Fun.const ()) with
  | Ok () -> assert (Atomic.get Miouc.tick = 0)
  | Error _ -> failwith "t23"
