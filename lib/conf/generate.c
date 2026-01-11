#define _GNU_SOURCE /* for linux */
#include <poll.h>
#include <stdio.h>
#include "miou_poll.h"

int main(void) {
  printf("let pollin = 0x%x\n", POLLIN);
  printf("let pollpri = 0x%x\n", POLLPRI);
  printf("let pollout = 0x%x\n", POLLOUT);
  printf("let pollerr = 0x%x\n", POLLERR);
  printf("let pollhup = 0x%x\n", POLLHUP);
  printf("let pollnval = 0x%x\n", POLLNVAL);
  printf("let sizeof_pollfd = 0x%x\n", sizeof(struct pollfd));
#if defined(HAS_PPOLL)
  printf("let has_ppoll = true\n");
#else
  printf("let has_ppoll = false\n");
#endif
  fflush(stdout);
  return 0;
}
