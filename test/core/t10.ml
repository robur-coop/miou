(* NOTE(dinosaure): This example is like [t08], we can not wait a task launch
   by another domain. It show up that a transmission of a promise from a domain
   to another is illegal. *)

let () =
  Miou.run @@ fun () ->
  let p = ref None in
  let a =
    Miou.call @@ fun () ->
    p := Some (Miou.call_cc (Fun.const ()));
    Miou.await_exn (Option.get !p)
  in
  let b =
    Miou.call @@ fun () ->
    let rec until () =
      match !p with Some p -> p | None -> until (Miou.yield ())
    in
    Miou.await_exn (until ())
  in
  Miou.await_exn a; Miou.await_exn b
