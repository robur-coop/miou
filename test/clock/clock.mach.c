#include <mach/mach.h>
#include <mach/mach_time.h>
#include <unistd.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#ifndef __unused
#define __unused(x) x __attribute((unused))
#endif

#define __unit() value __unused(unit)

static mach_timebase_info_data_t s = { 0 };

CAMLprim value
clock_mach_init(__unit ()) {
  if (mach_timebase_info (&s) != KERN_SUCCESS)
    caml_raise_sys_error(caml_copy_string("clock_mach_init: mach_timebase_info() failed"));
  if (s.denom == 0)
    caml_raise_sys_error(caml_copy_string("clock_mach_init: mach_timebase_info_data.denom is 0"));

  return Val_unit;
}

uint64_t
clock_mach_get_time(__unit ())
{
  return (mach_absolute_time() * s.numer / s.denom);
}
