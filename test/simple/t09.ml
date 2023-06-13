open Miou

let () =
  Miou.run @@ fun () ->
  ignore (Prm.call (fun () -> print_endline "Launched!"; Unix.sleepf 1.))
