(* NOTE(dinosaure): This test shows slightly more precise behaviour in relation
   to cancellation. It says that if a promise (even an almost-resolved one) is
   cancelled (like [t08]), we make a state transition. However, if the user has
   **already** had the result of this promise, we don't do it. *)

let prgm () =
  Miou.run @@ fun () ->
  let p = Miou.call (Fun.const ()) in
  Miou.await_exn p;
  Miou.cancel p;
  match Miou.await p with Error Miou.Cancelled -> () | _ -> failwith "t13"

let () =
  for _ = 0 to 100 do
    prgm ()
  done
