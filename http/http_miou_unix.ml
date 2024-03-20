let src = Logs.Src.create "http-miou-unix"

module Log = (val Logs.src_log src : Logs.LOG)

module TCP = struct
  type t = Miou_unix.file_descr
  type error = Unix.error * string * string

  let pp_error ppf (err, f, v) =
    Fmt.pf ppf "%s(%s): %s" f v (Unix.error_message err)

  let read flow buf ~off ~len =
    match Miou_unix.read flow buf off len with
    | len -> Ok len
    | exception Unix.Unix_error (Unix.ECONNRESET, _, _) -> Ok 0
    | exception Unix.Unix_error (err, f, v) -> Error (err, f, v)

  let full_write flow ({ Cstruct.len; _ } as cs) =
    let str = Cstruct.to_string cs in
    let rec go fd buf off len =
      if len = 0 then Ok ()
      else
        match Unix.select [] [ fd ] [] (-1.0) with
        | [], [ _ ], [] -> begin
            try
              let len' = Unix.single_write fd buf off len in
              go fd buf (off + len') (len - len')
            with
            | Unix.Unix_error (Unix.EINTR, _, _) -> go fd buf off len
            | Unix.Unix_error (err, f, v) -> Error (err, f, v)
          end
        | _ -> go fd buf off len
        | exception Unix.Unix_error (err, f, v) -> Error (err, f, v)
    in
    go (Miou_unix.to_file_descr flow) (Bytes.unsafe_of_string str) 0 len

  let writev flow css =
    let cs = Cstruct.concat css in
    full_write flow cs

  let close = Miou_unix.close

  let shutdown flow = function
    | `Recv ->
        Unix.shutdown (Miou_unix.to_file_descr flow) Unix.SHUTDOWN_RECEIVE
    | `Send -> Unix.shutdown (Miou_unix.to_file_descr flow) Unix.SHUTDOWN_SEND
end

module TLS = Tls_miou.Make (TCP)

let to_tls cfg ?host flow = TLS.client_of_flow cfg ?host flow

let epoch tls =
  match tls.TLS.state with
  | `End_of_input | `Error _ -> None
  | `Active tls -> (
      match Tls.Engine.epoch tls with
      | Error () -> assert false
      | Ok data -> Some data)
