#ifndef KILN_H
#define KILN_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct kiln_request kiln_request;
typedef enum {
  KILN_OK = 0,
  KILN_ERROR = 1,
  KILN_INTERNAL = 2,
  KILN_CANCELLED = 3,
  KILN_TRANSIENT = 75
} kiln_status;
typedef struct {
  /* Borrowed until kiln_request_free; len excludes the trailing NUL. */
  const char *data;
  size_t len;
  kiln_status status;
} kiln_result;

/* Copy a JSON request. Returns NULL on allocation failure or invalid pointer/
   length arguments. Each request may be passed to plan only once. */
kiln_request *kiln_request_create(const char *data, size_t len);
/* May be called from another thread while plan is running. */
void kiln_request_cancel(kiln_request *request);
/* Return JSON on success and failure. Failures contain success:false and logs
   with Level, Msg and DocsPath fields. Source paths are absolute. */
kiln_result kiln_plan(kiln_request *request);
/* Free only after plan and any concurrent cancellation calls finish. */
void kiln_request_free(kiln_request *request);

#ifdef __cplusplus
}
#endif

#endif
