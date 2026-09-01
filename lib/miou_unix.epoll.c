#include "conf/miou_poll.h"

#ifdef HAS_EPOLL
#define _GNU_SOURCE

#include <errno.h>
#include <string.h>
#include <sys/epoll.h>
#include <unistd.h>

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#ifndef __unused
#if defined(_MSC_VER) && _MSC_VER >= 1500
#define __unused(x)                                                            \
  __pragma(warning(push)) __pragma(warning(disable : 4189)) x __pragma(        \
      warning(pop))
#else
#define __unused(x) x __attribute__((unused))
#endif
#endif
#define __unit() value __unused(unit)

CAMLprim value miou_epoll_create(__unit()) {
  int fd;

  fd = epoll_create1(EPOLL_CLOEXEC);
  if (fd < 0)
    caml_uerror("epoll_create1", Nothing);

  return Val_int(fd);
}

CAMLprim value miou_epoll_ctl(value vepfd, value vop, value vfd,
                              value vevents) {
  CAMLparam4(vepfd, vop, vfd, vevents);
  struct epoll_event ev;
  int op, r;

  switch (Int_val(vop)) {
  case 0:
    op = EPOLL_CTL_ADD;
    break;
  case 1:
    op = EPOLL_CTL_MOD;
    break;
  default:
    op = EPOLL_CTL_DEL;
    break;
  }

  memset(&ev, 0, sizeof(ev));
  ev.events = (uint32_t)Int_val(vevents);
  ev.data.fd = Int_val(vfd);

  r = epoll_ctl(Int_val(vepfd), op, Int_val(vfd), &ev);
  if (r < 0)
    CAMLreturn(Val_int(errno));

  CAMLreturn(Val_int(0));
}

CAMLprim value miou_epoll_wait(value vepfd, value vbuf, value vmaxevents,
                               value vtimeoutns) {
  CAMLparam4(vepfd, vbuf, vmaxevents, vtimeoutns);
  struct epoll_event evs[256];
  struct timespec ts, *pts;
  int32_t *out;
  int epfd, maxevents, i, r;
  int64_t timeout_ns;

  epfd = Int_val(vepfd);
  maxevents = Int_val(vmaxevents);
  if (maxevents > 256)
    maxevents = 256;
  if (maxevents < 1)
    maxevents = 1;

  timeout_ns = Int64_val(vtimeoutns);
  if (timeout_ns < 0) {
    pts = NULL;
  } else {
    ts.tv_sec = (time_t)(timeout_ns / 1000000000);
    ts.tv_nsec = (long)(timeout_ns % 1000000000);
    pts = &ts;
  }

  if (pts != NULL && timeout_ns == 0)
    r = epoll_pwait2(epfd, evs, maxevents, pts, NULL);
  else {
    caml_enter_blocking_section();
    r = epoll_pwait2(epfd, evs, maxevents, pts, NULL);
    caml_leave_blocking_section();
  }

  if (r < 0) {
    if (errno == EINTR)
      CAMLreturn(Val_int(0));

    caml_uerror("epoll_pwait2", Nothing);
  }

  out = (int32_t *)Caml_ba_data_val(vbuf);
  for (i = 0; i < r; i++) {
    out[i * 2] = (int32_t)evs[i].data.fd;
    out[i * 2 + 1] = (int32_t)evs[i].events;
  }

  CAMLreturn(Val_int(r));
}

CAMLprim value miou_epoll_flag_in(__unit()) { return Val_int(EPOLLIN); }
CAMLprim value miou_epoll_flag_out(__unit()) { return Val_int(EPOLLOUT); }
CAMLprim value miou_epoll_flag_err(__unit()) { return Val_int(EPOLLERR); }
CAMLprim value miou_epoll_flag_hup(__unit()) { return Val_int(EPOLLHUP); }
CAMLprim value miou_epoll_flag_rdhup(__unit()) { return Val_int(EPOLLRDHUP); }
CAMLprim value miou_epoll_flag_oneshot(__unit()) {
  return Val_int(EPOLLONESHOT);
}

CAMLprim value /* noalloc */
miou_unix_epoll_max_open_files(__unit()) {
  return (Val_int(sysconf(_SC_OPEN_MAX)));
}

#endif /* HAS_EPOLL */
