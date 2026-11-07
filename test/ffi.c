#define _POSIX_C_SOURCE 200809L
#define _DARWIN_C_SOURCE
#include "kiln.h"
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define REQUIRE(test) do { if (!(test)) { fprintf(stderr, "line %d: %s\n", __LINE__, #test); abort(); } } while (0)

static void diagnostic(kiln_result result, kiln_status status) {
  if (result.status != status) fprintf(stderr, "status %d: %.*s\n", result.status, (int)result.len, result.data);
  REQUIRE(result.status == status && result.len != 0);
  REQUIRE(result.len == strlen(result.data));
  REQUIRE(strstr(result.data, "\"success\":false") != NULL);
  REQUIRE(strstr(result.data, "\"logs\":[{") != NULL);
  REQUIRE(strstr(result.data, "\"Level\":\"error\"") != NULL);
}

static void *invalid(void *unused) {
  (void)unused;
  kiln_request *request = kiln_request_create("{", 1);
  REQUIRE(request != NULL);
  diagnostic(kiln_plan(request), KILN_ERROR);
  diagnostic(kiln_plan(request), KILN_INTERNAL);
  kiln_request_free(request);
  return NULL;
}

int main(void) {
  diagnostic(kiln_plan(NULL), KILN_INTERNAL);
  /* Exercise native thread entry, then reuse the SDK from the main thread. */
  pthread_t thread;
  REQUIRE(pthread_create(&thread, NULL, invalid, NULL) == 0);
  REQUIRE(pthread_join(thread, NULL) == 0);
  invalid(NULL);
  kiln_request *cancelled = kiln_request_create("{}", 2);
  REQUIRE(cancelled != NULL);
  kiln_request_cancel(cancelled);
  diagnostic(kiln_plan(cancelled), KILN_CANCELLED);
  kiln_request_free(cancelled);
  return 0;
}
