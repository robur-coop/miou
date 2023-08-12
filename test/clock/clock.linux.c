#include <time.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#ifndef __unused
#define __unused(x) x __attribute((unused))
#endif

#define __unit() value __unused(unit)

CAMLprim value
clock_linux_get_time_bytecode(__unit ()) {
  struct timespec ts;

  (void) clock_gettime(CLOCK_MONOTONIC, &ts);

  return caml_copy_int64(ts.tv_sec * 1000000000LL + ts.tv_nsec);
}

CAMLprim value
clock_linux_get_time_native(__unit ()) {
  struct timespec ts;

  (void) clock_gettime(CLOCK_MONOTONIC, &ts);

  return (ts.tv_sec * 1000000000LL + ts.tv_nsec);
}
