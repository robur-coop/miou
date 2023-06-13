open Miou

let () = Random.self_init ()

exception Basic_failure

let () =
  Miou.run
    begin
      fun () ->
        let p =
          Prm.call_cc @@ fun () ->
          let child_of_p = Prm.call_cc (Fun.const 1) in
          if Random.bool () then (print_endline "Failed!"; raise Basic_failure);
          let value = Prm.await_exn child_of_p in
          value
        in
        Prm.await p
    end
  |> function
  | Ok 1 -> ()
  | Error Basic_failure -> ()
  | _ -> exit 1
