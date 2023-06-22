open Miou

let () =
  Miou.run @@ fun () ->
  let p = ref None in
  let a =
    Prm.call @@ fun () ->
    p := Some (Prm.call_cc (Fun.const ()));
    Prm.await_exn (Option.get !p)
  in
  let b =
    Prm.call @@ fun () ->
    let rec until () = match !p with Some p -> p | None -> until () in
    Prm.await_exn (until ())
  in
  Prm.await_all [ a; b ] |> ignore
