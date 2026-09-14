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

#include <errno.h>

int main(void) {
  struct epoll_event ev;
  struct timespec ts;
  int fd = epoll_create1(EPOLL_CLOEXEC);

  if (fd < 0)
    return (errno == ENOSYS);

  ts.tv_sec = 0; ts.tv_nsec = 0;
  ev.events = EPOLLIN | EPOLLONESHOT;
  ev.data.fd = 0;

  if (epoll_pwait2(fd, &ev, 1, &ts, NULL) < 0)
    return (errno == ENOSYS);

  return 0;
}
|c}

let epoll_is_implemented c =
  let split str = String.split_on_char ' ' str |> List.filter (( <> ) "") in
  let var name = Option.fold ~none:[] ~some:split (C.ocaml_config_var c name) in
  match var "c_compiler" with
  | [] -> true
  | cc :: cc_args -> begin
      let src = Filename.temp_file "has_epoll" ".c" in
      let exe = Filename.temp_file "has_epoll" ".exe" in
      let fn filename = try Sys.remove filename with _ -> () in
      let finally () = List.iter fn [ src; exe ] in
      Fun.protect ~finally @@ fun () ->
      let oc = open_out_bin src in
      output_string oc has_epoll_code;
      close_out oc;
      let args =
        cc_args
        @ var "ocamlc_cflags"
        @ var "ocamlc_cppflags"
        @ [ src; "-o"; exe ]
      in
      if not (C.Process.run_ok c cc args) then true
      else
        match C.Process.run c exe [] with
        | { C.Process.exit_code= 0; _ } -> true
        | _ -> false
        | exception _ -> false
    end

let () =
  C.main ~name:"select" @@ fun c ->
  let has_poll = C.c_test c has_poll_code in
  let has_epoll =
    (not (C.ocaml_config_var_exn c "system" = "win32"))
    && C.c_test c has_epoll_code
    && epoll_is_implemented c
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
