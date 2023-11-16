type _ Effect.t += Foo : unit Effect.t

let () =
  try
    Miou.run @@ fun () ->
    try Effect.perform Foo with Effect.Unhandled Foo -> failwith "t29"
  with Effect.Unhandled Foo -> ()
