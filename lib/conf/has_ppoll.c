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
