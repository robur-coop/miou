type _ Effect.t += Foo : unit Effect.t

let () =
  Miou.run @@ fun () -> try Effect.perform Foo with Effect.Unhandled Foo -> ()
