(* NOTE(dinosaure): We show up that [miou] take **randomly** a task. So we must
   have a chance to get [1] and [2]. The randomisation is important for a
   security point-of-view. It gives a chance to cover all cases also. *)

let prgm () =
  Miou.run @@ fun () ->
  let a = Miou.call_cc (Fun.const 1) in
  let b = Miou.call_cc (Fun.const 2) in
  Miou.await_first [ a; b ]

let rec until_its value =
  match prgm () with
  | Ok value' when value = value' -> ()
  | _ -> until_its value

let () = until_its 1; until_its 2
