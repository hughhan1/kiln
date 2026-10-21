#define _POSIX_C_SOURCE 200809L
#define _DARWIN_C_SOURCE
#include "kiln.h"
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>
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

static void transient(void) {
  int socket_fd = socket(AF_INET, SOCK_STREAM, 0);
  REQUIRE(socket_fd >= 0);
  struct sockaddr_in address = { .sin_family = AF_INET, .sin_addr.s_addr = htonl(INADDR_LOOPBACK) };
  REQUIRE(bind(socket_fd, (struct sockaddr *)&address, sizeof(address)) == 0);
  socklen_t size = sizeof(address);
  REQUIRE(getsockname(socket_fd, (struct sockaddr *)&address, &size) == 0);
  char proxy[64];
  snprintf(proxy, sizeof(proxy), "http://127.0.0.1:%u", ntohs(address.sin_port));
  REQUIRE(setenv("HTTPS_PROXY", proxy, 1) == 0);
  REQUIRE(setenv("NO_PROXY", "", 1) == 0);
  REQUIRE(setenv("no_proxy", "", 1) == 0);
  char root[] = "/tmp/kiln-ffi-XXXXXX";
  REQUIRE(mkdtemp(root) != NULL);
  char path[sizeof(root) + 32];
  snprintf(path, sizeof(path), "%s/package.json", root);
  FILE *package = fopen(path, "w");
  REQUIRE(package != NULL);
  REQUIRE(fputs("{}", package) >= 0);
  REQUIRE(fclose(package) == 0);
  char input[sizeof(root) + 32];
  int length = snprintf(input, sizeof(input), "{\"source\":\"%s\"}", root);
  REQUIRE(length > 0 && (size_t)length < sizeof(input));
  kiln_request *request = kiln_request_create(input, (size_t)length);
  REQUIRE(request != NULL);
  diagnostic(kiln_plan(request), KILN_TRANSIENT);
  kiln_request_free(request);
  REQUIRE(unlink(path) == 0);
  REQUIRE(rmdir(root) == 0);
  REQUIRE(close(socket_fd) == 0);
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
  transient();
  return 0;
}
