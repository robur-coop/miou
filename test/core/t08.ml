
let prgm () =
  Miou.run @@ fun () ->
  let p = Miou.call (Fun.const ()) in
  Miou.cancel p; Miou.await p

let () =
  for _ = 0 to 100 do
    match prgm () with Error Miou.Cancelled -> () | _ -> failwith "t08"
  done
