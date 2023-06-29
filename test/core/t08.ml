open Miou

let prgm () =
  Miou.run @@ fun () ->
  let p = Prm.call (Fun.const ()) in
  Prm.cancel p; Prm.await p

let () =
  for _ = 0 to 100 do
    match prgm () with Error Prm.Cancelled -> () | _ -> failwith "t08"
  done
