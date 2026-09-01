module C = struct
  external create : unit -> int = "miou_epoll_create"
  external ctl : int -> int -> Unix.file_descr -> int -> int = "miou_epoll_ctl"

  external wait :
       int
    -> (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
    -> int
    -> int64
    -> int = "miou_epoll_wait"

  external flag_in : unit -> int = "miou_epoll_flag_in" [@@noalloc]
  external flag_out : unit -> int = "miou_epoll_flag_out" [@@noalloc]
  external flag_err : unit -> int = "miou_epoll_flag_err" [@@noalloc]
  external flag_hup : unit -> int = "miou_epoll_flag_hup" [@@noalloc]
  external flag_rdhup : unit -> int = "miou_epoll_flag_rdhup" [@@noalloc]
  external flag_oneshot : unit -> int = "miou_epoll_flag_oneshot" [@@noalloc]
end

type t = int

module Flags = struct
  type t = int

  let epollin = C.flag_in ()
  let epollout = C.flag_out ()
  let epollerr = C.flag_err ()
  let epollhup = C.flag_hup ()
  let epollrdhup = C.flag_rdhup ()
  let epolloneshot = C.flag_oneshot ()
  let empty = 0
  let ( + ) = ( lor )
  let mem a b = a land b != 0
  let of_int = Fun.id
end

let () = assert (Obj.is_int (Obj.repr Unix.stdout))
let create () = C.create ()
let close t = Unix.close (Obj.magic t : Unix.file_descr) (* TODO *)
let add t fd flags = C.ctl t 0 fd flags
let upd t fd flags = C.ctl t 1 fd flags
let del t fd = C.ctl t 2 fd 0

type events = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t

let make_events n =
  Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout (n * 2)

type timeout = Infinite | No_wait | Nanoseconds of int64

let wait t events timeout =
  let timeout =
    match timeout with
    | Infinite -> Int64.minus_one
    | No_wait -> 0L
    | Nanoseconds ns -> if Int64.compare ns 0L < 0 then 0L else ns
  in
  let maxevents = Bigarray.Array1.dim events / 2 in
  C.wait t events maxevents timeout

let iter events n fn =
  for i = 0 to n - 1 do
    let fd : Unix.file_descr =
      Obj.magic (Int32.to_int (Bigarray.Array1.unsafe_get events (i * 2)))
    in
    let flags =
      Int32.to_int (Bigarray.Array1.unsafe_get events ((i * 2) + 1))
    in
    fn fd flags
  done
