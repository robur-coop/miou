type _ Effect.t += Foo : unit Effect.t

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

let () =
  Miou.run ~handler:{ Miou.handler } @@ fun () ->
  let rec go str n =
    if n >= 0 then begin
      Effect.perform Foo;
      print_endline str;
      go str (pred n)
    end
  in
  let prm0 = Miou.call_cc @@ fun () -> go "Hello" 1 in
  let prm1 = Miou.call_cc @@ fun () -> go "World" 1 in
  Miou.await_exn prm0; Miou.await_exn prm1
