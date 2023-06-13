#include <caml/memory.h>
#include <caml/domain.h>

CAMLprim value miou_interrupt(__unused(unit)) {
  return Val_bool(caml_incoming_interrupts_queued())
}
