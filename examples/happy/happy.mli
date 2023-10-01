include
  Dns_client.S
    with type io_addr = [ `Plaintext of Ipaddr.t * int ]
     and type +'a io = 'a

type daemon

type getaddrinfo = {
    getaddrinfo:
      'response 'a.
         'response Dns.Rr_map.key
      -> 'a Domain_name.t
      -> ('response, [ `Msg of string ]) result
}
[@@unboxed]

val stack :
     ?aaaa_timeout:int64
  -> ?connect_delay:int64
  -> ?connect_timeout:int64
  -> ?resolve_timeout:int64
  -> ?resolve_retries:int
  -> unit
  -> daemon * stack

val inject_resolver : getaddrinfo:getaddrinfo -> stack -> unit
(** [inject_resolver ~getaddrinfo stack] injects a DNS resolver into the given
    {i happy-eyeballs} [stack]. Initially, the {i happy-eyeballs} stack (created
    by {!val:stack}) can not resolve domain-name. When the user is able to
    resolve a domain-name (via the DNS protocol for example), he/she can
    {i inject} its resolver into the {i happy-eyeballs} stack.

    Only after injection the user can use {!val:connect_host} &
    {!val:connect_endpoint}. *)

val kill : daemon -> unit

val connect_ip :
     stack
  -> (Ipaddr.t * int) list
  -> ((Ipaddr.t * int) * Miou_unix.file_descr, [> `Msg of string ]) result
(** [connect_ip t addresses] establishes a connection to [addresses]. *)

val connect_host :
     stack
  -> [ `host ] Domain_name.t
  -> int list
  -> ((Ipaddr.t * int) * Miou_unix.file_descr, [> `Msg of string ]) result
(** [connect_host t host ports] establishes a connection to [host] on [ports]
    (tried in sequence).

    @raise Failure if [ports] is empty. *)

val connect_endpoint :
     stack
  -> string
  -> int list
  -> ((Ipaddr.t * int) * Miou_unix.file_descr, [> `Msg of string ]) result
(** [connect_endpoint t host ports] establishes a connection to [host] on
    [ports], which may be a host name or an IP address.

    @raise Failure if [ports] is the empty list. *)
