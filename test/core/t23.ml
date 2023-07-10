(* NOTE(dinosaure): We show up the basic [with_timeout] function. *)

exception Timeout

let with_timeout s fn =
  let p0 = Miou.call_cc fn in
  let p1 = Miou.call_cc @@ fun () -> Miouu.sleep s; raise Timeout in
  Miou.await_first [ p0; p1 ]

let () =
  let t0 = Unix.gettimeofday () in
  match Miouu.run @@ fun () -> with_timeout 10. (Fun.const ()) with
  | Ok () ->
      let t1 = Unix.gettimeofday () in
      assert (t1 -. t0 < 10.)
  | Error _ -> failwith "t23"
