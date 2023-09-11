type _ Effect.t += Foo : unit Effect.t

let prgm () =
  let prm = Miou.call @@ fun () -> Effect.perform Foo in
  Miou.await_exn prm

let handler fn v =
  let open Effect.Deep in
  let retc = Fun.id in
  let exnc = raise in
  let effc : type c. c Effect.t -> ((c, 'a) continuation -> 'b) option =
    function
    | Foo -> Some (fun k -> continue k ())
    | _ -> None
  in
  match_with fn v { retc; exnc; effc }

let () = handler (fun () -> Miou.run ~handler:{ Miou.handler } prgm) ()
