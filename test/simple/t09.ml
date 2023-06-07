open Miou

let () =
  Miou.run @@ fun () ->
  let _ = Prm.call (fun () -> print_endline "Launched!"; Unix.sleep 1) in
  ()
