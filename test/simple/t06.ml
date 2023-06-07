open Miou

let () = Random.self_init ()

let () =
  Miou.run (fun () ->
      Prm.await_first_exn
        [
          Prm.call (fun () ->
              let n = Random.int 5 in
              Unix.sleep n; 1)
        ; Prm.call (fun () ->
              let n = Random.int 5 in
              Unix.sleep n; 2)
        ])
  |> Format.printf "%d\n%!"
