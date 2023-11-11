(* NOTE(dinosaure): This code is given as an example into our documentation.
   We ensure that it terminates properly. *)

let global = ref None

let select ~poll:_ _ =
  match !global with
  | Some v -> [ Miou.task v (fun () -> global := None) ]
  | None -> []

let events _ = { Miou.interrupt= ignore; select }

let () =
  Miou.run ~events @@ fun () ->
  let p = Miou.make (Fun.const ()) in
  global := Some p;
  Miou.suspend p
