open Miou

let sleepers = Hashtbl.create 0x100

let sleep until =
  let return = Fun.const () in
  let promise = Miou.make return in
  Hashtbl.add sleepers (Miou.uid promise) (promise, until);
  Miou.suspend promise

exception Infinite_loop

let () =
  Printexc.register_printer @@ function
  | Infinite_loop -> Some "Infinite_loop"
  | _ -> None

let infinite_loop _ = raise Infinite_loop
let dummy _ = { select= infinite_loop; interrupt= ignore }
let () = Miou.(run ~events:dummy @@ fun () -> sleep 1.; ())
