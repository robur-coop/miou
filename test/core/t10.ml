
let () =
  Miou.run @@ fun () ->
  let p = ref None in
  let a =
    Miou.call @@ fun () ->
    p := Some (Miou.call_cc (Fun.const ()));
    Miou.await_exn (Option.get !p)
  in
  let b =
    Miou.call @@ fun () ->
    let rec until () = match !p with Some p -> p | None -> until () in
    Miou.await_exn (until ())
  in
  Miou.await_all [ a; b ] |> ignore
