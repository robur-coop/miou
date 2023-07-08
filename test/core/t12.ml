let prgm () =
  Miou.run @@ fun () ->
  let p = Miou.call_cc (Fun.const ()) in
  Miou.yield (); Miou.cancel p; Miou.await p

let () = match prgm () with Error Miou.Cancelled -> () | _ -> failwith "t12"
