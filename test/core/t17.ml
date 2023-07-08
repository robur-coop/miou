let global = ref None

let select () =
  match !global with
  | Some v -> [ Miou.task v (fun () -> global := None) ]
  | None -> []

let events _ = { Miou.interrupt= ignore; select }
let or_raise = function Ok v -> v | Error exn -> raise exn

let () =
  Miou.run ~events @@ fun () ->
  let p = Miou.make (Fun.const ()) in
  global := Some p;
  or_raise (Miou.suspend p)
