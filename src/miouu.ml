open Miou

type file_descr = { fd: Unix.file_descr; non_blocking: bool }

let of_file_descr ?(non_blocking = true) fd =
  if non_blocking then Unix.set_nonblock fd else Unix.clear_nonblock fd;
  { fd; non_blocking }

let to_file_descr { fd; _ } = fd

type unix_scheduler = {
    rd: (Unix.file_descr, unit Prm.t) Hashtbl.t
  ; wr: (Unix.file_descr, unit Prm.t) Hashtbl.t
  ; sleepers: (Id.t, float * unit Prm.t) Hashtbl.t
}

let dom =
  let make () =
    {
      rd= Hashtbl.create 0x100
    ; wr= Hashtbl.create 0x100
    ; sleepers= Hashtbl.create 0x100
    }
  in
  let dom = Domain.DLS.new_key make in
  fun () -> Domain.DLS.get dom

let minimum sleepers =
  let fold uid (until, prm) = function
    | Some (_, until', _) when until < until' -> Some (uid, until, prm)
    | Some _ as acc -> acc
    | None -> Some (uid, until, prm)
  in
  Hashtbl.fold fold sleepers None

let sleeper () =
  let dom = dom () in
  match minimum dom.sleepers with
  | Some (uid, _, prm) ->
      let k () = Hashtbl.remove dom.sleepers uid in
      Some [ Miou.syscall prm k ]
  | None -> None

let blocking_read fd =
  let dom = dom () in
  let prm = Prm.make ~return:(Fun.const ()) in
  Hashtbl.add dom.rd fd prm; Prm.await_exn prm

let blocking_write fd =
  let dom = dom () in
  let prm = Prm.make ~return:(Fun.const ()) in
  Hashtbl.add dom.wr fd prm; Prm.await_exn prm

let rec read ({ fd; non_blocking } as file_descr) buf ~off ~len =
  if non_blocking then
    match Unix.read fd buf off len with
    | exception Unix.(Unix_error (EINTR, _, _)) -> read file_descr buf ~off ~len
    | exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
        blocking_read fd;
        read file_descr buf ~off ~len
    | value -> value
  else
    let rec go () =
      match Unix.read fd buf off len with
      | exception Unix.(Unix_error (EINTR, _, _)) -> go ()
      | value -> value
    in
    blocking_read fd; go ()

let rec write ({ fd; non_blocking } as file_descr) str ~off ~len =
  if non_blocking then
    match Unix.write fd (Bytes.unsafe_of_string str) off len with
    | exception Unix.(Unix_error (EINTR, _, _)) ->
        write file_descr str ~off ~len
    | exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
        blocking_write fd;
        write file_descr str ~off ~len
    | len' when len' < len ->
        write file_descr str ~off:(off + len') ~len:(len - len')
    | _ -> ()
  else
    let rec go () =
      match Unix.write fd (Bytes.unsafe_of_string str) off len with
      | exception Unix.(Unix_error (EINTR, _, _)) -> go ()
      | len' when len' < len ->
          write file_descr str ~off:(off + len') ~len:(len - len')
      | _ -> ()
    in
    blocking_write fd; go ()

let rec connect ({ fd; non_blocking } as file_descr) sockaddr =
  if not non_blocking then
    invalid_arg
      "Miouu.connect: we expect a file descriptor in the non-blocking mode";
  match Unix.connect fd sockaddr with
  | () -> ()
  | exception Unix.(Unix_error (EINTR, _, _)) -> connect file_descr sockaddr
  | exception Unix.(Unix_error (EINPROGRESS, _, _)) -> (
      blocking_write fd;
      match Unix.getsockopt_error fd with
      | None -> ()
      | Some err -> raise (Unix.Unix_error (err, "connect", "")))

let rec accept ?cloexec ({ fd; non_blocking } as file_descr) =
  if non_blocking then (
    match Unix.accept ?cloexec fd with
    | exception Unix.(Unix_error (EINTR, _, _)) -> accept ?cloexec file_descr
    | exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
        blocking_read fd; accept ?cloexec file_descr
    | fd, sockaddr ->
        Unix.set_nonblock fd;
        let file_descr = { fd; non_blocking= true } in
        let res =
          Own.own ~finally:(fun { fd; _ } -> Unix.close fd) file_descr
        in
        (res, file_descr, sockaddr))
  else
    let rec go () =
      match Unix.accept ?cloexec fd with
      | exception Unix.(Unix_error (EINTR, _, _)) -> go ()
      | fd, sockaddr ->
          Unix.set_nonblock fd;
          let file_descr = { fd; non_blocking= true } in
          let res =
            Own.own ~finally:(fun { fd; _ } -> Unix.close fd) file_descr
          in
          (res, file_descr, sockaddr)
    in
    blocking_read fd; go ()

let events () =
  let dom = dom () in
  let rds = Hashtbl.fold (fun fd _ acc -> fd :: acc) dom.rd [] in
  let wrs = Hashtbl.fold (fun fd _ acc -> fd :: acc) dom.wr [] in
  let ts =
    Option.map (fun (_, until, _) -> until) (minimum dom.sleepers) |> function
    | Some ts -> ts
    | None when rds = [] && wrs = [] -> 0.
    | None -> -1.
  in
  let t0 = Unix.gettimeofday () in
  match Unix.select rds wrs [] ts with
  | exception Unix.(Unix_error (EINTR, _, _)) -> None
  | [], [], _ ->
      let t1 = Unix.gettimeofday () in
      let syscalls = sleeper () in
      Hashtbl.filter_map_inplace
        (fun _ (until, prm) ->
          let until' = until -. (t1 -. t0) in
          if until' > 0. then Some (until', prm) else None)
        dom.sleepers;
      syscalls
  | rds, wrs, [] ->
      let t1 = Unix.gettimeofday () in
      Hashtbl.filter_map_inplace
        (fun _ (until, prm) ->
          let until' = until -. (t1 -. t0) in
          if until' > 0. then Some (until', prm) else None)
        dom.sleepers;
      let syscalls = [] in
      let syscalls =
        List.fold_left
          (fun acc fd ->
            let prm = Hashtbl.find dom.rd fd in
            Miou.syscall prm (fun () -> Hashtbl.remove dom.rd fd) :: acc)
          syscalls rds
      in
      let syscalls =
        List.fold_left
          (fun acc fd ->
            let prm = Hashtbl.find dom.wr fd in
            Miou.syscall prm (fun () -> Hashtbl.remove dom.wr fd) :: acc)
          syscalls wrs
      in
      Some syscalls
  | _ -> None

let run ?g fn = Miou.run ~events ?g fn

let sleep until =
  let dom = dom () in
  let prm = Prm.make ~return:(Fun.const ()) in
  Hashtbl.add dom.sleepers (Prm.uid prm) (until, prm);
  Prm.await_exn prm
