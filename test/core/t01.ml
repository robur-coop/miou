(* NOTE(dinosaure): We show up the [yield] behavior here. This example is given
   in the documentation and we ensure that the result is determined. The output
   must be:
   World
   Hello
   Hello
   World *)

let prgm0 () =
  Miou.run @@ fun () ->
  let p = Miou.call_cc @@ fun () -> print_endline "Hello" in
  print_endline "World"; Miou.await_exn p

let prgm1 () =
  Miou.run @@ fun () ->
  let p = Miou.call_cc @@ fun () -> print_endline "Hello" in
  Miou.yield (); print_endline "World"; Miou.await_exn p

let () = prgm0 (); prgm1 ()
