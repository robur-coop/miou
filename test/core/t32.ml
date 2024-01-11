let program () =
  let prm = Miou.call (Fun.const ()) in
  Miou.await_exn prm

let () =
  try
    Miou.run ~domains:0 program;
    exit 1
  with Miou.No_domain_available -> ()

let program () =
  let prm = Miou.call @@ fun () -> Miou.await_exn (Miou.call (Fun.const ())) in
  Miou.await_exn prm

let () =
  try
    Miou.run ~domains:1 program;
    exit 1
  with Miou.No_domain_available -> ()
