#define _POSIX_C_SOURCE 200809L
#define _DARWIN_C_SOURCE
#define _XOPEN_SOURCE 700
#include "kiln.h"
#include <stdint.h>
#include <pthread.h>
#include <signal.h>
#include <stdatomic.h>
#include <stdlib.h>
#include <string.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/printexc.h>
#include <caml/threads.h>

struct kiln_request {
  atomic_bool cancelled;
  atomic_bool started;
  size_t len;
  char *output;
  char input[];
};

#define DIAGNOSTIC(message) "{\"success\":false,\"logs\":[{\"Level\":\"error\",\"Msg\":\"" message "\",\"DocsPath\":\"\"}]}"
#define FAILURE(message, status) ((kiln_result){ DIAGNOSTIC(message), sizeof(DIAGNOSTIC(message)) - 1, status })

static pthread_once_t initialized = PTHREAD_ONCE_INIT;
static const value *entry;
static _Thread_local kiln_request *current;
static _Thread_local int initial_thread;

static int restore_stack(stack_t original) {
  if (original.ss_flags & SS_DISABLE) original.ss_size = SIGSTKSZ;
  return sigaltstack(&original, NULL);
}

static void initialize(void) {
  char *argv[] = { "kiln", NULL };
  value result = caml_startup_exn(argv);
  if (!Is_exception_result(result)) entry = caml_named_value("kiln.execute");
  initial_thread = 1;
  caml_release_runtime_system();
}

kiln_request *kiln_request_create(const char *data, size_t len) {
  if ((data == NULL && len != 0) || len > SIZE_MAX - sizeof(kiln_request)) return NULL;
  kiln_request *request = calloc(1, sizeof(*request) + len);
  if (request == NULL) return NULL;
  if (len != 0) memcpy(request->input, data, len);
  request->len = len;
  atomic_init(&request->cancelled, 0);
  atomic_init(&request->started, 0);
  return request;
}

void kiln_request_cancel(kiln_request *request) {
  atomic_store_explicit(&request->cancelled, 1, memory_order_relaxed);
}

CAMLprim value kiln_cancelled(value unit) {
  (void)unit;
  return Val_bool(atomic_load_explicit(&current->cancelled, memory_order_relaxed));
}

CAMLprim value kiln_input(value unit) {
  (void)unit;
  return caml_alloc_initialized_string(current->len, current->input);
}

static kiln_result invoke(kiln_request *request) {
  value result = caml_callback_exn(*entry, Val_unit);
  if (Is_exception_result(result))
    return FAILURE("cannot allocate OCaml error diagnostic", KILN_INTERNAL);
  value bytes = Field(result, 1);
  size_t len = caml_string_length(bytes);
  request->output = malloc(len + 1);
  if (request->output == NULL) return FAILURE("out of memory", KILN_INTERNAL);
  memcpy(request->output, String_val(bytes), len);
  request->output[len] = '\0';
  return (kiln_result){ request->output, len, (kiln_status)Int_val(Field(result, 0)) };
}

kiln_result kiln_plan(kiln_request *request) {
  if (request == NULL) return FAILURE("null request", KILN_INTERNAL);
  if (atomic_exchange_explicit(&request->started, 1, memory_order_relaxed))
    return FAILURE("request already used", KILN_INTERNAL);
  stack_t original;
  if (sigaltstack(NULL, &original) != 0)
    return FAILURE("cannot read signal stack", KILN_INTERNAL);
  pthread_once(&initialized, initialize);
  if (entry == NULL || (!initial_thread && !caml_c_thread_register())) {
    restore_stack(original);
    return FAILURE("cannot initialize OCaml thread", KILN_INTERNAL);
  }
  if (restore_stack(original) != 0) {
    if (!initial_thread) caml_c_thread_unregister();
    return FAILURE("cannot restore signal stack", KILN_INTERNAL);
  }
  caml_acquire_runtime_system();
  current = request;
  kiln_result output = invoke(request);
  current = NULL;
  caml_release_runtime_system();
  if (!initial_thread) caml_c_thread_unregister();
  if (atomic_load_explicit(&request->cancelled, memory_order_relaxed))
    return FAILURE("operation cancelled", KILN_CANCELLED);
  return output;
}

void kiln_request_free(kiln_request *request) {
  if (request == NULL) return;
  free(request->output);
  free(request);
}
