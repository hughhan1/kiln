#include <node_api.h>
#include <filesystem>
#include <string>

static napi_value answer(napi_env env, napi_callback_info) {
  const std::string value = std::filesystem::current_path().string() + std::to_string(42);
  napi_value result;
  napi_create_string_utf8(env, value.c_str(), value.size(), &result);
  return result;
}

static napi_value init(napi_env env, napi_value exports) {
  napi_value function;
  napi_create_function(env, "answer", NAPI_AUTO_LENGTH, answer, nullptr, &function);
  napi_set_named_property(env, exports, "answer", function);
  return exports;
}

NAPI_MODULE(addon, init)
