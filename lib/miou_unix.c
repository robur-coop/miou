/* Copyright (c) 2023 Christiano Haesbaert haesbaert@haesbaert.org
 * SPDX-License-Identifier: ISC
 * Copyright (c) 2025 Romain Calascibetta <romain.calascibetta@gmail.com>
 */

#include "conf/miou_poll.h"

#ifdef HAS_POLL
#define _GNU_SOURCE

#include <errno.h>
#include <poll.h>
#include <signal.h>

#include <caml/bigarray.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#include "miou_ppoll.h"

CAMLextern int caml_convert_signal_number(int);

CAMLprim value miou_unix_poll(value v_fds, value v_nfds, value v_timo) {
  CAMLparam3(v_fds, v_nfds, v_timo);
  struct pollfd *fds;
  nfds_t nfds;
  int timo;
  int r;

  fds = Caml_ba_data_val(v_fds);
  nfds = Int_val(v_nfds);
  timo = Int_val(v_timo);

  caml_enter_blocking_section();
  r = poll(fds, nfds, timo);
  caml_leave_blocking_section();

  if (r == -1)
    uerror("poll", Nothing); /* allocation */

  CAMLreturn(Val_int(r));
}

#ifdef HAS_PPOLL
static void decode_sigset(value vset, sigset_t *set) {
  sigemptyset(set);
  for (; vset != Val_emptylist; vset = Field(vset, 1)) {
    int sig = caml_convert_signal_number(Int_val(Field(vset, 0)));
    sigaddset(set, sig);
  }
}
#endif

#define S_IN_NS 1000000000LL
CAMLprim value miou_unix_ppoll(value v_fds, value v_nfds, value v_timo,
                               value v_sigmask) {
#ifdef HAS_PPOLL
  CAMLparam4(v_fds, v_nfds, v_timo, v_sigmask);
  struct pollfd *fds;
  struct timespec *timo;
  struct timespec ts;
  sigset_t *psigmask, sigmask;
  nfds_t nfds;
  int64_t timo64;
  int r;

  fds = Caml_ba_data_val(v_fds);
  nfds = Int_val(v_nfds);
  timo64 = Int64_val(v_timo);

  if (timo64 == -1LL)
    timo = NULL;
  else {
    ts.tv_sec = (time_t)(timo64 / S_IN_NS);
    ts.tv_nsec = (time_t)(timo64 % S_IN_NS);
    timo = &ts;
  }

  if (v_sigmask == Val_emptylist)
    psigmask = NULL;
  else {
    decode_sigset(v_sigmask, &sigmask);
    psigmask = &sigmask;
  }

  caml_enter_blocking_section();
  r = ppoll(fds, nfds, timo, psigmask);
  caml_leave_blocking_section();

  if (r == -1)
    uerror("ppoll", Nothing);

  CAMLreturn(Val_int(r));
#else  /* HAS_PPOLL */
  errno = ENOSYS;
  uerror("ppoll", Nothing);
#endif /* HAS_PPOLL */
}
#undef S_IN_NS

#define pollfd_of_index(vfds, vindex)                                          \
  ((struct pollfd *)Caml_ba_data_val(vfds) + (Int_val(vindex)))

CAMLprim value /* noalloc */
miou_unix_poll_set_index(value v_fds, value v_index, value v_fd,
                         value v_events) {
  struct pollfd *pfd = pollfd_of_index(v_fds, v_index);
  pfd->fd = Int_val(v_fd);
  pfd->events = Int_val(v_events);
  return Val_unit;
}

CAMLprim value miou_unix_poll_init(value v_fds, value v_maxfds) {
  CAMLparam2(v_fds, v_maxfds);
  struct pollfd *pfd = pollfd_of_index(v_fds, Val_int(0));
  int maxfds = Int_val(v_maxfds);
  int i;

  for (i = 0; i < maxfds; i++, pfd++) {
    pfd->fd = -1;
    pfd->events = 0;
  }

  CAMLreturn(Val_unit);
}

CAMLprim value /* noalloc */
miou_unix_poll_get_revents(value v_fds, value v_index) {
  struct pollfd *pfd = pollfd_of_index(v_fds, v_index);
  return (Val_int(pfd->revents));
}

CAMLprim value /* noalloc */
miou_unix_poll_get_fd(value v_fds, value v_index) {
  struct pollfd *pfd = pollfd_of_index(v_fds, v_index);
  return (Val_int(pfd->fd));
}

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

CAMLprim value /* noalloc */
miou_unix_poll_max_open_files(value __unused(v_unit)) {
  return (Val_int(sysconf(_SC_OPEN_MAX)));
}

#endif /* HAS_POLL */
