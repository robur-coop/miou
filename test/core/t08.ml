(* NOTE(dinosaure): We show up that a cancellation, even for an almost-resolved
   task terminates with [Miou.Cancelled]. [Miou.cancel] does a state transition
   even if the task is resolved. We try 100 times to ensure this behavior. *)

let prgm () =
  Miou.run @@ fun () ->
  let p = Miou.call (Fun.const ()) in
  Miou.cancel p; Miou.await p

let () =
  for _ = 0 to 100 do
    match prgm () with Error Miou.Cancelled -> () | _ -> failwith "t08"
  done
