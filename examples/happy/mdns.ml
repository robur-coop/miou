include Dns_client.Make (Transport)

let format =
  {|
The format of an IP address and optional port is:
- '[::1]:port' for an IPv6 address, or
- '127.0.0.1:port' for an IPv4 address.

The format of a nameserver is:
- 'udp:IP' where the first element is the string "udp" and the [IP] as described
  above (port defaults to 53): UDP packets to the provided IP address will be
  sent from a random source port;
- 'tcp:IP' where the first element is the string "tcp" and the [IP] as described
  above (port defaults to 53): a TCP connection to the provided IP address will
  be established;
- 'tls:IP' where the first element is the string "tls", the [IP] as described
  above (port defaults to 853): a TCP connection will be established, on top of
  which a TLS handshake with the authenticator
  (https://github.com/mirage/ca-certs-nss) will be done (which checks for the
  IP address being in the certificate as SubjectAlternativeName);
- 'tls:IP!hostname' where the first element is the string "tls",
  the [IP] as described above (port defaults to 853), the [hostname] a host name
  used for the TLS authentication: a TCP connection will be established, on top
  of which a TLS handshake with the authenticator
  (https://github.com/mirage/ca-certs-nss) will be done;
- 'tls:IP!hostname!authenticator' where the first element is the string "tls",
  the [IP] as described above (port defaults to 853), the [hostname] a host name
  used for the TLS authentication, and the [authenticator] an X509
  authenticator: a TCP connection will be established, on top of which a TLS
  handshake with the authenticator will be done.
|}

let nameserver_of_string str =
  let ( let* ) = Result.bind in
  begin
    match String.split_on_char ':' str with
    | "tcp" :: nameserver ->
        let str = String.concat ":" nameserver in
        let* ipaddr, port = Ipaddr.with_port_of_string ~default:53 str in
        Ok (`Tcp, `Plaintext (ipaddr, port))
    | "udp" :: nameserver ->
        let str = String.concat ":" nameserver in
        let* ipaddr, port = Ipaddr.with_port_of_string ~default:53 str in
        Ok (`Udp, `Plaintext (ipaddr, port))
    | _ -> Error (`Msg ("Unable to decode nameserver " ^ str))
  end
  |> Result.map_error (function `Msg e -> `Msg (e ^ format))

let connect ?cache_size ?edns ?(nameservers = []) ?timeout () =
  let nameservers =
    List.map
      (fun nameserver ->
        match nameserver_of_string nameserver with
        | Ok nameserver -> nameserver
        | Error (`Msg err) -> invalid_arg err)
      nameservers
  in
  let tcp, udp =
    List.fold_left
      (fun (tcp, udp) -> function
        | `Tcp, a -> (a :: tcp, udp)
        | `Udp, a -> (tcp, a :: udp))
      ([], []) nameservers
  in
  let nameservers =
    match (tcp, udp) with
    | [], [] -> None
    | [], _ :: _ -> invalid_arg "We don't handle UDP packets"
    | tcp, _ -> Some (`Tcp, tcp)
  in
  create ?cache_size ?edns ?nameservers ?timeout ()

let kill t = Transport.kill (transport t)
