open Miou

module Timer = struct
  type t = { uid: Id.t; until: float }

  let compare a b = Stdlib.compare a.until b.until
  let dummy = { uid= Id.null; until= max_float }
end

module Sleep = Binary_heap.Make (Timer)

type file_descr = { fd: Unix.file_descr; non_blocking: bool }

type unix_scheduler = {
    rd: (Unix.file_descr, rd) Hashtbl.t
  ; wr: (Unix.file_descr, wr) Hashtbl.t
  ; sleepers: (Id.t, unit Sysc.t) Hashtbl.t * Sleep.t
}

and rd =
  [ `Read of int Sysc.t * bytes * int * int
  | `Accept of (file_descr * Unix.sockaddr) Sysc.t * bool option ]

and wr =
  [ `Write of int Sysc.t * string * int * int | `In_progress of unit Sysc.t ]

let dom =
  let make () =
    {
      rd= Hashtbl.create 256
    ; wr= Hashtbl.create 256
    ; sleepers= (Hashtbl.create 256, Sleep.create ~dummy:Timer.dummy 256)
    }
  in
  let dom = Domain.DLS.new_key make in
  fun () -> Domain.DLS.get dom

let rec read ({ fd; non_blocking } as file_descr) buf ~off ~len =
  if non_blocking then
    match Unix.read fd buf off len with
    | exception Unix.(Unix_error (EINTR, _, _)) -> read file_descr buf ~off ~len
    | exception Unix.(Unix_error ((EWOULDBLOCK | EAGAIN), _, _)) ->
        let dom = dom () in
        let var = Sysc.make () in
        Hashtbl.add dom.rd fd (`Read (var, buf, off, len));
        Sysc.await var
    | len -> Ok len
    | exception exn -> Error exn
  else
    let dom = dom () in
    let var = Sysc.make () in
    Hashtbl.add dom.rd fd (`Read (var, buf, off, len));
    Sysc.await var

let rec write ({ fd; non_blocking } as file_descr) str ~off ~len =
  if non_blocking then
    match Unix.single_write fd (Bytes.unsafe_of_string str) off len with
    | exception Unix.(Unix_error (EINTR, _, _)) ->
        write file_descr str ~off ~len
    | exception Unix.(Unix_error ((EWOULDBLOCK | EAGAIN), _, _)) ->
        let dom = dom () in
        let var = Sysc.make () in
        Hashtbl.add dom.wr fd (`Write (var, str, off, len));
        Sysc.await var
    | len -> Ok len
    | exception exn -> Error exn
  else
    let dom = dom () in
    let var = Sysc.make () in
    Hashtbl.add dom.wr fd (`Write (var, str, off, len));
    Sysc.await var

let rec accept ?cloexec ({ fd; non_blocking } as file_descr) =
  if non_blocking then (
    match Unix.accept ?cloexec fd with
    | exception Unix.(Unix_error (EINTR, _, _)) -> accept ?cloexec file_descr
    | exception Unix.(Unix_error ((EWOULDBLOCK | EAGAIN), _, _)) ->
        let dom = dom () in
        let var = Sysc.make () in
        Hashtbl.add dom.rd fd (`Accept (var, cloexec));
        Sysc.await var
    | fd, sockaddr ->
        Unix.set_nonblock fd;
        Ok ({ fd; non_blocking= true }, sockaddr))
  else
    let dom = dom () in
    let var = Sysc.make () in
    Hashtbl.add dom.rd fd (`Accept (var, cloexec));
    Sysc.await var

let rec connect ({ fd; non_blocking } as file_descr) sockaddr =
  if non_blocking then
    match Unix.connect fd sockaddr with
    | () -> Ok ()
    | exception Unix.(Unix_error (EINTR, _, _)) -> connect file_descr sockaddr
    | exception Unix.(Unix_error (EINPROGRESS, _, _)) -> (
        match Unix.getsockopt_error fd with
        | None ->
            let dom = dom () in
            let var = Sysc.make () in
            Hashtbl.add dom.wr fd (`In_progress var);
            Sysc.await var
        | Some uerr -> Error (Unix.Unix_error (uerr, "connect", "")))
  else
    Error (Invalid_argument "connect() requires a non-blocking socket")

let invalid_arg fmt = Format.kasprintf (fun msg -> Invalid_argument msg) fmt

let rec socket ty = function
  | `Host (name, port) -> (
      match Unix.gethostbyname name with
      | exception Not_found -> Error (invalid_arg "%s: host not found" name)
      | h ->
          let v = `Sockaddr (Unix.ADDR_INET (h.h_addr_list.(0), port)) in
          socket ty v)
  | `Sockaddr sockaddr -> (
      let domain = Unix.domain_of_sockaddr sockaddr in
      match Unix.socket ~cloexec:true domain ty 0 with
      | exception exn -> Error exn
      | fd -> (
          match Unix.set_nonblock fd with
          | exception exn ->
              (try Unix.close fd with _ -> ());
              Error exn
          | () -> Ok { fd; non_blocking= true }))

let next_sleeper (_, sleepers) =
  match Sleep.minimum sleepers with
  | { until; _ } -> until
  | exception Binary_heap.Empty -> 0.

let resolve_sleeper () =
  let dom = dom () in
  let tbl, sleepers = dom.sleepers in
  match Sleep.minimum sleepers with
  | { uid; _ } ->
      (* NOTE(dinosaure): we can be worried by the fact that a function [k]
         removes few things from our global (to the domain) state and think
         about the possible next call to [events] but actually, [Some ...] will
         fill the internal [miou]'s scheduler and will certainly execute our
         new task **before** the next [event] call.

         Again, [event] is only called when the internal list of tasks is
         **empty** and we currently fill it via [Some ...]. In other words,
         it's impossible to resend the same [k] before the first is executed. *)
      let syscall = Hashtbl.find tbl uid in
      let k () = Sleep.remove sleepers in
      Some [ Miou.syscall syscall k ]
  | exception Binary_heap.Empty -> None

let rec blocking_read fd buf ~off ~len () =
  match Unix.read fd buf off len with
  | exception Unix.(Unix_error (EINTR, _, _)) ->
      blocking_read fd buf ~off ~len ()
  | len -> len

let rec blocking_write fd str ~off ~len () =
  match Unix.single_write fd (Bytes.unsafe_of_string str) off len with
  | exception Unix.(Unix_error (EINTR, _, _)) ->
      blocking_write fd str ~off ~len ()
  | len -> len

let rec blocking_accept ?cloexec fd () =
  match Unix.accept ?cloexec fd with
  | exception Unix.(Unix_error (EINTR, _, _)) -> blocking_accept ?cloexec fd ()
  | fd, sockaddr ->
      Unix.set_nonblock fd;
      ({ fd; non_blocking= true }, sockaddr)

let events () =
  let dom = dom () in
  let timeout = next_sleeper dom.sleepers in
  let rds = Hashtbl.fold (fun fd _ acc -> fd :: acc) dom.rd [] in
  match Unix.select rds [] [] timeout with
  | exception Unix.(Unix_error (EINTR, _, _)) -> None
  | [], [], _ -> resolve_sleeper ()
  | rds, wrs, _ ->
      let readers fd =
        match Hashtbl.find dom.rd fd with
        | `Read (var, buf, off, len) ->
            Miou.syscall var (blocking_read fd buf ~off ~len)
        | `Accept (var, cloexec) ->
            Miou.syscall var (blocking_accept ?cloexec fd)
      in
      let writers fd =
        match Hashtbl.find dom.wr fd with
        | `Write (var, str, off, len) ->
            Miou.syscall var (blocking_write fd str ~off ~len)
        | `In_progress var -> Miou.syscall var (Fun.const ())
      in
      let readers = List.map readers rds in
      let writers = List.map writers wrs in
      Some (readers @ writers)

let run ?g fn = Miou.run ~events ?g fn

let sleep until =
  let dom = dom () in
  let tbl, sleepers = dom.sleepers in
  let var = Sysc.make () in
  Sleep.add sleepers { uid= Sysc.uid var; until };
  Hashtbl.add tbl (Sysc.uid var) var;
  Sysc.await var
