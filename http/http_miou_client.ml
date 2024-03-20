open Http_miou_unix

module Httpaf_Client_connection = struct
  include Httpaf.Client_connection

  let yield_reader _ = assert false

  let next_read_operation t =
    (next_read_operation t :> [ `Close | `Read | `Yield ])
end

module A = Runtime.Make (TLS) (Httpaf_Client_connection)
module B = Runtime.Make (TCP) (Httpaf_Client_connection)

type error = [ `V1 of Httpaf.Client_connection.error | `Protocol of string ]
type response = [ `V1 of Httpaf.Response.t ]

type ('resp, 'body) version =
  | V1 : (Httpaf.Response.t, [ `write ] Httpaf.Body.t) version

type 'resp await = unit -> ('resp, error) result

type 'acc process =
  | Process :
      ('resp, 'body) version * ('resp * 'acc) await * 'body
      -> 'acc process

let http_1_1_response_handler ~f acc =
  let acc = ref acc in
  let response = ref None in
  let go resp body orphans =
    let rec on_eof () = Runtime.terminate orphans
    and on_read bstr ~off ~len =
      let str = Bigstringaf.substring bstr ~off ~len in
      acc := f (`V1 resp) !acc str;
      Httpaf.Body.schedule_read body ~on_read ~on_eof
    in
    response := Some (`V1 resp);
    Httpaf.Body.schedule_read body ~on_read ~on_eof
  in
  let response_handler resp body = Runtime.flat_tasks (go resp body) in
  (response_handler, response, acc)

let http_1_1_error_handler () =
  let error = ref None in
  let error_handler = function
    | `Exn (Runtime.Flow msg) -> error := Some (`Protocol msg)
    | err -> error := Some (`V1 err)
  in
  (error_handler, error)

let run ~f acc config flow request =
  match (flow, config, request) with
  | `Tls flow, `V1 config, `V1 request ->
      let read_buffer_size = config.Httpaf.Config.read_buffer_size in
      let response_handler, response, acc = http_1_1_response_handler ~f acc in
      let error_handler, error = http_1_1_error_handler () in
      let body, conn =
        Httpaf.Client_connection.request ~config request ~error_handler
          ~response_handler
      in
      let prm = A.run conn ~read_buffer_size flow in
      let await () =
        match (Miou.await_exn prm, !error, !response) with
        | _, Some error, _ -> Error error
        | Error exn, _, _ -> Error (`V1 (`Exn exn))
        | Ok (), None, Some (`V1 resp) -> Ok (resp, !acc)
        | Ok (), None, (Some (`V2 _) | None) -> assert false
      in
      Process (V1, await, body)
  | `Tcp flow, `V1 config, `V1 request ->
      let read_buffer_size = config.Httpaf.Config.read_buffer_size in
      let response_handler, response, acc = http_1_1_response_handler ~f acc in
      let error_handler, error = http_1_1_error_handler () in
      let body, conn =
        Httpaf.Client_connection.request ~config request ~error_handler
          ~response_handler
      in
      let prm = B.run conn ~read_buffer_size flow in
      let await () =
        match (Miou.await_exn prm, !error, !response) with
        | _, Some error, _ -> Error error
        | Error exn, _, _ -> Error (`V1 (`Exn exn))
        | Ok (), None, Some (`V1 resp) -> Ok (resp, !acc)
        | Ok (), None, (Some (`V2 _) | None) -> assert false
      in
      Process (V1, await, body)
