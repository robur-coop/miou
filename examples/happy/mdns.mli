include module type of Dns_client.Make (Transport)

val connect :
     ?cache_size:int
  -> ?edns:[ `None | `Auto | `Manual of Dns.Edns.t ]
  -> ?nameservers:string list
  -> ?timeout:int64
  -> unit
  -> t

val kill : t -> (unit, exn) result
