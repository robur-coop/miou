open Miou

let prgm () =
  Miou.run @@ fun () ->
  let a = Prm.call_cc (Fun.const 1) in
  let b = Prm.call_cc (Fun.const 2) in
  Prm.await_first [ a; b ]

let rec until_its value =
  match prgm () with
  | Ok value' when value = value' -> ()
  | _ -> until_its value

let () = until_its 1; until_its 2
