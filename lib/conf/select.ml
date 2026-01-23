module C = Configurator.V1

let has_poll_code =
  {c|#include <poll.h>
 #include <stddef.h>
 #include <strings.h>
 
 int main(void) {
   struct pollfd fds;
   
   bzero(&fds, sizeof(fds));

   return (poll(&fds, 0, 0));
 }
 |c}

let () =
  C.main ~name:"select" @@ fun c ->
  let has_poll = C.c_test c has_poll_code in
  C.C_define.gen_header_file c ~fname:"miou_poll.h"
    [ ("HAS_POLL", Switch has_poll) ];
  if has_poll then begin
    C.Flags.write_sexp "modules.sexp"
      [ "miou_unix"; "miou_poll"; "miou_poll_config" ];
    C.Flags.write_lines "impl.out" [ "poll" ]
  end
  else begin
    C.Flags.write_sexp "modules.sexp" [ "miou_unix" ];
    C.Flags.write_lines "impl.out" [ "select" ]
  end
