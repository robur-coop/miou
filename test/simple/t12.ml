open Miou

let () =
  let r =
    Miou.run @@ fun () ->
    let v = Prm.call_cc (Fun.const 1) in
    yield (); Prm.cancel v; Prm.await v
  in
  match r with Ok 1 -> () | _ -> exit 1
