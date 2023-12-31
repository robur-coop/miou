type _ Effect.t += Foo : unit Effect.t

let prgm () =
  let prm = Miou.call @@ fun () -> Effect.perform Foo in
  Miou.await_exn prm

let handler_foo fn v =
  let open Effect.Deep in
  let retc = Fun.id in
  let exnc = raise in
  let effc : type c. c Effect.t -> ((c, 'a) continuation -> 'b) option =
    function
    | Foo -> Some (fun k -> continue k ())
    | _ -> None
  in
  match_with fn v { retc; exnc; effc }

let () =
  Miou.run ~handler:{ Miou.handler= handler_foo } @@ fun () ->
  let prm = Miou.call @@ fun () -> Effect.perform Foo in
  Miou.await_exn prm

let () =
  Miou.run ~handler:{ Miou.handler= handler_foo } @@ fun () ->
  let prm = Miou.call_cc @@ fun () -> Effect.perform Foo in
  Miou.await_exn prm

type _ Effect.t += Bar : unit Effect.t

let handler_bar fn v =
  let open Effect.Deep in
  let retc = Fun.id in
  let exnc = raise in
  let effc : type c. c Effect.t -> ((c, 'a) continuation -> 'b) option =
    function
    | Bar -> Some (fun k -> continue k ())
    | _ -> None
  in
  match_with fn v { retc; exnc; effc }

let ( <.> ) { Miou.handler= foo } { Miou.handler= bar } =
  { Miou.handler= (fun fn v -> (foo (bar fn)) v) }

let () =
  let foo = { Miou.handler= handler_foo } in
  let bar = { Miou.handler= handler_bar } in
  Miou.run ~handler:(foo <.> bar) @@ fun () ->
  let prm0 = Miou.call @@ fun () -> Effect.perform Foo in
  let prm1 = Miou.call @@ fun () -> Effect.perform Bar in
  Miou.await_exn prm0; Miou.await_exn prm1
