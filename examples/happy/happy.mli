include
  Dns_client.S
    with type io_addr = [ `Plaintext of Ipaddr.t * int ]
     and type +'a io = 'a

type daemon

val daemon : timeout:int64 -> daemon * stack
val kill : daemon -> unit
