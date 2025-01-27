open Miou

let get, set =
  let make () = Hashtbl.create 0x100 in
  let key = Stdlib.Domain.DLS.new_key make in
  let get () = Stdlib.Domain.DLS.get key in
  let set value = Stdlib.Domain.DLS.set key value in
  (get, set)

let sleep until =
  let syscall = Miou.syscall () in
  let sleepers = get () in
  let fn () =
    Hashtbl.add sleepers (Miou.uid syscall) (syscall, until);
    set sleepers
  in
  Miou.suspend ~fn syscall

let select ~block:_ _ =
  let sleepers = get () in
  let min =
    Hashtbl.fold
      (fun uid (syscall, until) -> function
        | Some (_uid', _syscall', until') when until < until' ->
            Some (uid, syscall, until)
        | Some _ as acc -> acc
        | None -> Some (uid, syscall, until))
      sleepers None
  in
  match min with
  | None -> []
  | Some (_, _, until) ->
      let until = Float.min until 0.100 in
      Unix.sleepf until;
      Hashtbl.filter_map_inplace
        (fun _ (syscall, until') ->
          Some (syscall, Float.max 0. (until' -. until)))
        sleepers;
      let cs = ref [] in
      Hashtbl.filter_map_inplace
        (fun _ (syscall, until) ->
          if until <= 0. then begin
            cs := Miou.signal syscall :: !cs;
            None
          end
          else Some (syscall, until))
        sleepers;
      set sleepers;
      !cs

let events _ = { select; interrupt= ignore; finaliser= ignore }

let () =
  let t0 = Unix.gettimeofday () in
  let () =
    Miou.run ~events @@ fun () ->
    let a = Miou.async (fun () -> sleep 1.) in
    let b = Miou.async (fun () -> sleep 2.) in
    Miou.await_all [ a; b ]
    |> List.iter @@ function Ok () -> () | Error exn -> raise exn
  in
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 < 3.)
