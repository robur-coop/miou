let () =
  Miou.run @@ fun () ->
  let prm =
    Miou.call @@ fun () ->
    let _ =
      Miou.call @@ fun () ->
      while true do
        Miou.yield ()
      done
    in
    failwith "Foo"
  in
  match Miou.await prm with
  | Error (Failure foo) when foo = "Foo" -> ()
  | _ -> failwith "Unexpected result"
