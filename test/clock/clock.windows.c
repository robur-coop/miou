#include <windows.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#ifndef __unused
#define __unused(x) x __attribute((unused))
#endif

#define __unit() value __unused(unit)

static LARGE_INTEGER frequency;

CAMLprim value
clock_windows_init(__unit ()) {
  QueryPerformanceFrequence(&frequency);
  frequency.QuadPart = 1000000000L / frequency.QuadPart;

  return Val_unit;
}

uint64_t
clock_windows_get_time(__unit ()) {
  LARGE_INTEGER now;

  QueryPerformanceCounter(&now);

  return (now.QuadPart * frequency.QuadPart);
}
