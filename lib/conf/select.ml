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

let has_epoll_code =
  {c|#define _GNU_SOURCE
#include <sys/epoll.h>
#include <stddef.h>

int main(void) {
  struct epoll_event ev;
  struct timespec ts;
  int fd = epoll_create1(EPOLL_CLOEXEC);

  ts.tv_sec = 0; ts.tv_nsec = 0;
  ev.events = EPOLLIN | EPOLLONESHOT;
  ev.data.fd = 0;

  return (epoll_pwait2(fd, &ev, 1, &ts, NULL));
}
|c}

let () =
  C.main ~name:"select" @@ fun c ->
  let has_poll = C.c_test c has_poll_code in
  let has_epoll =
    (not (C.ocaml_config_var_exn c "system" = "win32"))
    && C.c_test c has_epoll_code
  in
  C.C_define.gen_header_file c ~fname:"miou_poll.h"
    [ ("HAS_POLL", Switch has_poll); ("HAS_EPOLL", Switch has_epoll) ];
  if has_epoll then begin
    C.Flags.write_sexp "modules.sexp" [ "miou_unix"; "miou_epoll" ];
    C.Flags.write_lines "impl.out" [ "epoll" ]
  end
  else if has_poll then begin
    C.Flags.write_sexp "modules.sexp"
      [ "miou_unix"; "miou_poll"; "miou_poll_config" ];
    C.Flags.write_lines "impl.out" [ "poll" ]
  end
  else begin
    C.Flags.write_sexp "modules.sexp" [ "miou_unix" ];
    C.Flags.write_lines "impl.out" [ "select" ]
  end
