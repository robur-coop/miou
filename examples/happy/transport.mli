include
  Dns_client.S
    with type io_addr = [ `Plaintext of Ipaddr.t * int ]
     and type +'a io = 'a
     and type stack = unit

val kill : t -> (unit, exn) result
