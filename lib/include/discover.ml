module C = Configurator.V1

let has_ppoll_code =
  {c|
#define _GNU_SOURCE /* for linux */
#include <poll.h>
#include <stddef.h>
#include <strings.h>

int main(void) {
  struct pollfd fds;
  struct timespec ts;

  bzero(&fds, sizeof(fds));
  bzero(&ts, sizeof(ts));

  return (ppoll(&fds, 0, &ts, NULL));
}
|c}

let () =
  C.main ~name:"discover" @@ fun c ->
  let has_ppoll = C.c_test c has_ppoll_code in
  C.C_define.gen_header_file c ~fname:"miou_poll.h"
    [ ("HAS_PPOLL", Switch has_ppoll) ];
  let has_list = [ Format.asprintf "let has_ppoll = %b" has_ppoll ] in
  let[@ocamlformat "disable"] defs =
    C.C_define.import c ~includes:[ "poll.h" ]
      C.C_define.Type.
        [
          ("POLLIN", Int)
        ; ("POLLPRI", Int)
        ; ("POLLOUT", Int)
        ; ("POLLERR", Int)
        ; ("POLLHUP", Int)
        ; ("POLLNVAL", Int)
        ; ("sizeof(struct pollfd)", Int)
        ]
  in
  let fn = function
    | "sizeof(struct pollfd)", C.C_define.Value.Int v ->
        Format.asprintf "let sizeof_pollfd = 0x%x" v
    | name, C.C_define.Value.Int v ->
        Format.asprintf "let %s = 0x%x" (String.lowercase_ascii name) v
    | _ -> assert false
  in
  let defs = List.map fn defs in
  C.Flags.write_lines "miou_poll_config.ml" (defs @ has_list)
