open Miou

let () =
  let r =
    Miou.run @@ fun () ->
    let v = Prm.call_cc (Fun.const 1) in
    (* XXX(dinosaure): here we give a chance for [v] to be resolved. Then, we
       [cancel] it and [await] it. [cancel] should not do a transition state and
       [await] should return [Ok 1]. *)
    Prm.yield (); Prm.cancel v; Prm.await v
  in
  match r with Ok 1 -> () | _ -> exit 1
