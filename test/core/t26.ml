open Miou

let sleepers = Hashtbl.create 0x100

let sleep until =
  let return = Fun.const () in
  let promise = Miou.make return in
  Hashtbl.add sleepers (Miou.uid promise) (promise, until);
  match Miou.suspend promise with Ok () -> () | Error exn -> raise exn

let dummy _ = { select= Fun.const []; interrupt= ignore }
let () = Miou.(run ~events:dummy @@ fun () -> sleep 1.; ())
