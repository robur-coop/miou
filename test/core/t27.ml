(* NOTE(dinosaure): this test shows the order chosen by Miou
   to execute tasks. *)

let () =
  Miou.run @@ fun () ->
  let v = ref 0 in
  let p0 =
    Miou.call_cc @@ fun () ->
    Miou.yield ();
    Miou.yield ();
    v := 1
  in
  let p1 =
    Miou.call_cc @@ fun () ->
    Miou.yield ();
    v := 2
  in
  ignore (Miou.await_all [ p0; p1 ]);
  Format.printf "%d\n%!" !v
