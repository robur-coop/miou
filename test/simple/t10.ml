open Miou

let () =
  Miou.run @@ fun () ->
  let v = Atomic.make None in
  let a =
    Prm.call (fun () ->
        let prm = Prm.call_cc (Fun.const ()) in
        Atomic.set v (Some prm))
  in
  let b =
    Prm.call (fun () ->
        let rec go () =
          match Atomic.get v with
          | Some prm -> Prm.await_exn prm
          | None -> go ()
        in
        go ())
  in
  Prm.await_exn a; Prm.await_exn b
