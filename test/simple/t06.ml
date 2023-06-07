open Miou

let () = Random.self_init ()

let rec random_int ?not_equal_to max =
  match not_equal_to with
  | None -> Random.int max
  | Some n ->
      if n >= max then invalid_arg "random_int";
      let m = Random.int max in
      if n = m then
        random_int ~not_equal_to:n max
      else
        m

let () =
  let a = random_int 5 in
  let b = random_int ~not_equal_to:a 5 in
  let t0 = Unix.gettimeofday () in
  let () =
    Miou.run @@ fun () ->
    Prm.await_first_exn
      [ Prm.call (fun () -> Unix.sleep a); Prm.call (fun () -> Unix.sleep b) ]
  in
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 < Float.of_int (max a b))
