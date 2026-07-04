#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <sys/time.h>

CAMLprim value datascript_now_seconds(value unit) {
  (void)unit;
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return caml_copy_double((double)tv.tv_sec + ((double)tv.tv_usec / 1000000.0));
}
