type buffer =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Config = Miou_poll_config

module C = struct
  external poll : buffer -> int -> int -> int = "miou_unix_poll"
  external ppoll : buffer -> int -> int64 -> int list -> int = "miou_unix_ppoll"

  external set_index : buffer -> int -> Unix.file_descr -> int -> unit
    = "miou_unix_poll_set_index"
  [@@noalloc]

  external init : buffer -> int -> unit = "miou_unix_poll_init"

  external get_revents : buffer -> int -> int = "miou_unix_poll_get_revents"
  [@@noalloc]

  external get_fd : buffer -> int -> Unix.file_descr = "miou_unix_poll_get_fd"
  [@@noalloc]

  external max_open_files : unit -> int = "miou_unix_poll_max_open_files"
  [@@noalloc]
end

module Flags = struct
  type t = int

  let pollin = Config.pollin
  let pollpri = Config.pollpri
  let pollout = Config.pollout
  let pollerr = Config.pollerr
  let pollhup = Config.pollhup
  let pollnval = Config.pollnval
  let empty = 0
  let ( + ) = ( lor )
  let mem a b = a land b != 0
  let to_int = Fun.id
  let of_int = Fun.id
end

let has_ppoll = Config.has_ppoll
let invalid_fd : Unix.file_descr = Obj.magic (-1)

type t = { buffer: buffer; maxfds: int }
type poll_timeout = Infinite | No_wait | Milliseconds of int

let poll t used timeout =
  let timeout =
    match timeout with Infinite -> -1 | No_wait -> 0 | Milliseconds ms -> ms
  in
  C.poll t.buffer used timeout

type ppoll_timeout = Infinite | No_wait | Nanoseconds of int64

let ppoll t used timeout sigmask =
  let timeout =
    match timeout with
    | Infinite -> Int64.minus_one
    | No_wait -> Int64.zero
    | Nanoseconds timo -> timo
  in
  C.ppoll t.buffer used timeout sigmask

let ppoll_or_poll t used (timeout : ppoll_timeout) =
  if has_ppoll then ppoll t used timeout []
  else
    let timeout : poll_timeout =
      match timeout with
      | Infinite -> Infinite
      | No_wait -> No_wait
      | Nanoseconds timo_ns ->
          Milliseconds Int64.(to_int (div (add timo_ns 999_999L) 1_000_000L))
    in
    poll t used timeout

let guard_index t index =
  if index >= t.maxfds || index < 0 then
    invalid_arg "Miou_poll: index out of bounds"

let set_index t index fd events =
  guard_index t index;
  C.set_index t.buffer index fd events

let invalidate_index t index =
  guard_index t index;
  C.set_index t.buffer index invalid_fd 0

let get_revents t index =
  guard_index t index;
  C.get_revents t.buffer index

let get_fd t index = guard_index t index; C.get_fd t.buffer index

let create ?(maxfds = C.max_open_files ()) () =
  let len = maxfds * Config.sizeof_pollfd in
  let buffer = Bigarray.(Array1.create char c_layout len) in
  let t = { buffer; maxfds } in
  C.init buffer maxfds; t

let maxfds { maxfds; _ } = maxfds

let iter t nready (fn : int -> Unix.file_descr -> Flags.t -> unit) =
  let rec go index nready =
    if nready > 0 then begin
      let fd = get_fd t index in
      let revents = get_revents t index in
      if fd <> invalid_fd && revents != 0 then begin
        fn index fd revents;
        go (index + 1) (nready - 1)
      end
      else go (index + 1) nready
    end
  in
  go 0 nready
