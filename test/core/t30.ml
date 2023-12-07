let () =
  Miou.run @@ fun () ->
  let p = Miou.call (Fun.const ()) in
  begin
    match Miou.await_one [ p; p ] with Ok () -> () | Error exn -> raise exn
  end;
  begin
    match Miou.await_first [ p; p ] with Ok () -> () | Error exn -> raise exn
  end
