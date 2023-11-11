(* NOTE(dinosaure): This test is like [t08] but with [Miou.call_cc]. We must
   use [Miou.yield] to give an opportunity to resolve [p], we cancel it and
   check that the result is [Miou.Cancelled]. Unlike [t08], we don't need to
   test this behaviour several times, as the use of [Miou.call_cc] and
   [Miou.yield] makes the execution of this code deterministic. *)

let prgm () =
  Miou.run @@ fun () ->
  let p = Miou.call_cc (Fun.const ()) in
  Miou.cancel p; Miou.await p

let () = match prgm () with Error Miou.Cancelled -> () | _ -> failwith "t12"
