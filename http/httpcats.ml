module Flow = Flow
module Runtime = Runtime
module Client = Http_miou_client

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let decode_host_port str =
  match String.split_on_char ':' str with
  | [] -> Error (`Msg "Empty host part")
  | [ host ] -> Ok (host, None)
  | [ host; "" ] -> Ok (host, None)
  | hd :: tl -> (
      let port, host =
        match List.rev (hd :: tl) with
        | hd :: tl -> (hd, String.concat ":" (List.rev tl))
        | _ -> assert false
      in
      try Ok (host, Some (int_of_string port))
      with _ -> Error (`Msg "Couln't decode port"))

let decode_user_pass up =
  match String.split_on_char ':' up with
  | [ user; pass ] -> Ok (user, Some pass)
  | [ user ] -> Ok (user, None)
  | _ -> assert false

type uri =
  bool * string * (string * string option) option * string * int option * string

let decode_uri uri =
  (* proto :// user : pass @ host : port / path *)
  let ( >>= ) = Result.bind in
  match String.split_on_char '/' uri with
  | proto :: "" :: user_pass_host_port :: path ->
      (if String.equal proto "http:" then Ok ("http", false)
       else if String.equal proto "https:" then Ok ("https", true)
       else Error (`Msg "Unknown protocol"))
      >>= fun (scheme, is_tls) ->
      (match String.split_on_char '@' user_pass_host_port with
      | [ host_port ] -> Ok (None, host_port)
      | [ user_pass; host_port ] ->
          decode_user_pass user_pass >>= fun up -> Ok (Some up, host_port)
      | _ -> Error (`Msg "Couldn't decode URI"))
      >>= fun (user_pass, host_port) ->
      decode_host_port host_port >>= fun (host, port) ->
      Ok (is_tls, scheme, user_pass, host, port, "/" ^ String.concat "/" path)
  | [ user_pass_host_port ] ->
      (match String.split_on_char '@' user_pass_host_port with
      | [ host_port ] -> Ok (None, host_port)
      | [ user_pass; host_port ] ->
          decode_user_pass user_pass >>= fun up -> Ok (Some up, host_port)
      | _ -> Error (`Msg "Couldn't decode URI"))
      >>= fun (user_pass, host_port) ->
      decode_host_port host_port >>= fun (host, port) ->
      Ok (false, "", user_pass, host, port, "/")
  | user_pass_host_port :: path ->
      (match String.split_on_char '@' user_pass_host_port with
      | [ host_port ] -> Ok (None, host_port)
      | [ user_pass; host_port ] ->
          decode_user_pass user_pass >>= fun up -> Ok (Some up, host_port)
      | _ -> Error (`Msg "Couldn't decode URI"))
      >>= fun (user_pass, host_port) ->
      decode_host_port host_port >>= fun (host, port) ->
      Ok (false, "", user_pass, host, port, "/" ^ String.concat "/" path)
  | _ -> Error (`Msg "Could't decode URI on top")

let add_authentication ?(meth = `Basic) ~add headers user_pass =
  match (user_pass, meth) with
  | None, _ -> headers
  | Some (user, Some pass), `Basic ->
      let data = Base64.encode_string (user ^ ":" ^ pass) in
      let str = "Basic " ^ data in
      add headers "authorization" str
  | Some (user, None), `Basic ->
      let data = Base64.encode_string user in
      let str = "Basic " ^ data in
      add headers "authorization" str

let user_agent = "http-client/%%VERSION_NUM%%"

let prep_http_1_1_headers headers host user_pass blen =
  let headers = Httpaf.Headers.of_list headers in
  let add = Httpaf.Headers.add_unless_exists in
  let headers = add headers "user-agent" user_agent in
  let headers = add headers "host" host in
  let headers = add headers "connection" "close" in
  let headers =
    add headers "content-length" (string_of_int (Option.value ~default:0 blen))
  in
  add_authentication ~add headers user_pass

module Version = Httpaf.Version
module Status = H2.Status
module Headers = H2.Headers

type response = {
    version: Version.t
  ; status: Status.t
  ; reason: string
  ; headers: Headers.t
}

type error =
  [ `V1 of Httpaf.Client_connection.error
  | `Protocol of string
  | `Msg of string
  | `Tls of Http_miou_unix.TLS.error ]

let pp_error ppf = function
  | `Protocol msg -> Fmt.string ppf msg
  | `Msg msg -> Fmt.string ppf msg
  | `Tls err -> Http_miou_unix.TLS.pp_error ppf err
  | `V1 (`Malformed_response msg) ->
      Fmt.pf ppf "Malformed HTTP/1.1 response: %s" msg
  | `V1 (`Invalid_response_body_length _resp) ->
      Fmt.pf ppf "Invalid response body length"
  | `V1 (`Exn exn) | `V2 (`Exn exn) ->
      Fmt.pf ppf "Got an unexpected exception: %S" (Printexc.to_string exn)

let from_httpaf response =
  {
    version= response.Httpaf.Response.version
  ; status= (response.Httpaf.Response.status :> H2.Status.t)
  ; reason= response.Httpaf.Response.reason
  ; headers=
      H2.Headers.of_list
        (Httpaf.Headers.to_list response.Httpaf.Response.headers)
  }

let single_http_1_1_request ?(config = Httpaf.Config.default) flow user_pass
    host meth path headers contents f acc =
  let contents_length = Option.map String.length contents in
  let headers = prep_http_1_1_headers headers host user_pass contents_length in
  let request = Httpaf.Request.create ~headers meth path in
  let f response acc str =
    let[@warning "-8"] (`V1 response : Client.response) = response in
    f (from_httpaf response) acc str
  in
  match Client.run ~f acc (`V1 config) flow (`V1 request) with
  | Process (V1, await, body) -> (
      let go orphans =
        Option.iter (Httpaf.Body.write_string body) contents;
        Runtime.terminate orphans
      in
      Runtime.flat_tasks go;
      match await () with
      | Ok (response, acc) -> Ok (from_httpaf response, acc)
      | Error (#Client.error as err) -> Error (err :> error))

let alpn_protocol = function
  | `Tcp _ -> None
  | `Tls tls -> (
      match Http_miou_unix.epoch tls with
      | Some { Tls.Core.alpn_protocol= Some "http/1.1"; _ } -> Some `HTTP_1_1
      | Some { Tls.Core.alpn_protocol= None; _ } -> None
      | Some { Tls.Core.alpn_protocol= Some _; _ } -> None
      | None -> None)

let connect ?port ?tls_config ~happy_eyeballs host =
  let port =
    match (port, tls_config) with
    | None, None -> 80
    | None, Some _ -> 443
    | Some port, _ -> port
  in
  match (Happy.connect_endpoint happy_eyeballs host [ port ], tls_config) with
  | Ok ((_ipaddr, _port), file_descr), None -> Ok (`Tcp file_descr)
  | Ok ((_ipaddr, _port), file_descr), Some tls_config ->
      let ( >>= ) = Result.bind in
      Http_miou_unix.to_tls tls_config file_descr
      |> Result.map_error (fun err -> `Tls err)
      >>= fun file_descr -> Ok (`Tls file_descr)
  | (Error _ as err), _ -> err

let single_request ~happy_eyeballs ?http_config tls_config ~meth ~headers ?body
    uri f acc =
  let ( let* ) = Result.bind in
  let ( let+ ) x f = Result.map f x in
  let* tls, _scheme, user_pass, host, port, path = decode_uri uri in
  let* tls_config =
    if tls then
      let+ tls_config = tls_config in
      let host =
        let* domain_name = Domain_name.of_string host in
        Domain_name.host domain_name
      in
      match (tls_config, host) with
      | `Custom cfg, _ -> Some cfg
      | `Default cfg, Ok host -> Some (Tls.Config.peer cfg host)
      | `Default cfg, _ -> Some cfg
    else Ok None
  in
  let* flow = connect ?port ?tls_config ~happy_eyeballs host in
  match (alpn_protocol flow, http_config) with
  | (Some `HTTP_1_1 | None), Some (`HTTP_1_1 config) ->
      single_http_1_1_request ~config flow user_pass host meth path headers body
        f acc
  | (Some `HTTP_1_1 | None), None ->
      single_http_1_1_request flow user_pass host meth path headers body f acc
  | _ -> assert false

let resolve_location ~uri ~location =
  match String.split_on_char '/' location with
  | "http:" :: "" :: _ -> Ok location
  | "https:" :: "" :: _ -> Ok location
  | "" :: "" :: _ ->
      let schema = String.sub uri 0 (String.index uri '/') in
      Ok (schema ^ location)
  | "" :: _ -> begin
      match String.split_on_char '/' uri with
      | schema :: "" :: user_pass_host_port :: _ ->
          Ok (String.concat "/" [ schema; ""; user_pass_host_port ^ location ])
      | _ -> error_msgf "Expected an absolute uri, got: %S" uri
    end
  | _ -> error_msgf "Unknown location (relative path): %S" location

let request ?config ?tls_config ?authenticator ?(meth = `GET) ?(headers = [])
    ?body ?(max_redirect = 5) ?(follow_redirect = true) ~resolver:happy_eyeballs
    ~f ~uri acc =
  let tls_config =
    match tls_config with
    | Some cfg -> Ok (`Custom cfg)
    | None ->
        let alpn_protocols =
          match config with
          | None -> [ "http/1.1" ]
          | Some (`H2 _) -> [ "h2" ]
          | Some (`HTTP_1_1 _) -> [ "http/1.1" ]
        and authenticator =
          match authenticator with
          | None -> Ca_certs.authenticator ()
          | Some authenticator -> Ok authenticator
        in
        Result.map
          (fun authenticator ->
            `Default (Tls.Config.client ~alpn_protocols ~authenticator ()))
          authenticator
  in
  let http_config =
    match config with
    | Some (`H2 cfg) -> Some (`V2 cfg)
    | Some (`HTTP_1_1 cfg) -> Some (`V1 cfg)
    | None -> None
  in
  if not follow_redirect then
    single_request ~happy_eyeballs ?http_config tls_config ~meth ~headers ?body
      uri f acc
  else
    let ( >>= ) = Result.bind in
    let rec follow_redirect count uri =
      if count = 0 then Error (`Msg "Redirect limit exceeded")
      else
        match
          single_request ~happy_eyeballs ?http_config tls_config ~meth ~headers
            ?body uri f acc
        with
        | Error _ as err -> err
        | Ok (resp, body) ->
            if Status.is_redirection resp.status then
              match Headers.get resp.headers "location" with
              | Some location ->
                  resolve_location ~uri ~location >>= fun uri ->
                  follow_redirect (pred count) uri
              | None -> Ok (resp, body)
            else Ok (resp, body)
    in
    follow_redirect max_redirect uri
