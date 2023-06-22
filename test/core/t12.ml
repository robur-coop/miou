open Miou

let prgm () =
  Miou.run @@ fun () ->
  let p = Prm.call_cc (Fun.const ()) in
  yield (); Prm.cancel p; Prm.await p

let () = match prgm () with Ok () -> () | Error _ -> failwith "t12"
