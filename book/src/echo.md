# An echo server with Miou

```ocaml
let rec clean_up orphans =
  match Miou.care orphans with
  | None | Some None -> ()
  | Some (Some prm) ->
    match Miou.await prm with
    | Ok () -> clean_up orphans
    | Error exn -> raise exn

let server () =
  let rec go orphans =
    clean_up orphans;
    let client, address_of_client = Miou_unix.accept socket in
    ignore (Miou.call_cc ~orphans (echo client));
    go orphans in
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, 3000) in
  Unix.bind socket sockaddr;
  Unix.listen socket 64;
  go (Miou.orphans ())
```
