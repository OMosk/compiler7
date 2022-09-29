#define STB_SPRINTF_IMPLEMENTATION
#include "stb_sprintf.h"
#include <stdint.h>

typedef long i64;
typedef double f64;

// Needed for #c_call and #support_library test
i64 sum_i64(i64 a, i64 b) {
  return a + b;
}

// Needed for #c_call and #support_library test
f64 sum_f64(f64 a, f64 b) {
  return a + b;
}

#define PREALLOCATED_HEAP_SPACE (1024 * 1024 * 100)
static char arena[PREALLOCATED_HEAP_SPACE];
static unsigned long arena_used;

char *c7_dynamic_allocation(i64 size_to_allocate) {
  size_to_allocate = size_to_allocate / 16;
  size_to_allocate = size_to_allocate + 1;
  size_to_allocate = size_to_allocate * 16; // align to 16 byte boundary

  char *result = arena + arena_used;
  arena_used = arena_used + size_to_allocate;

  return result;
}

typedef struct {
  char *data;
  i64 len;
} C7_String;

C7_String *integer_to_string(i64 n) {
  char buffer[64];
  int len = stbsp_snprintf(buffer, sizeof(buffer), "%ld", n);

  C7_String *result = (C7_String *) c7_dynamic_allocation(sizeof(C7_String));
  result->data = c7_dynamic_allocation(len + 1);
  for (int i = 0; i <= len; ++i) {
    result->data[i] = buffer[i];
  }
  result->len = len;
  return result;
}

C7_String *double_to_string(double d) {
  char buffer[64];
  int len = stbsp_snprintf(
    buffer,
    sizeof(buffer),
    "%f",
    d);

  C7_String *c7_string_result = (C7_String *) c7_dynamic_allocation(sizeof(C7_String));
  c7_string_result->data = c7_dynamic_allocation(len + 1);
  for (int i = 0; i <= len; ++i) {
    c7_string_result->data[i] = buffer[i];
  }
  c7_string_result->len = len;
  return c7_string_result;
}

C7_String *c7_string_concat(C7_String *a, C7_String *b) {
  C7_String *c7_string_result = (C7_String *) c7_dynamic_allocation(sizeof(C7_String));
  c7_string_result->data = c7_dynamic_allocation(a->len + b->len + 1);
  for (int i = 0; i <= a->len; ++i) {
    c7_string_result->data[i] = a->data[i];
  }
  for (int i = 0; i <= b->len; ++i) {
    c7_string_result->data[a->len + i] = b->data[i];
  }
  c7_string_result->len = a->len + b->len;
  return c7_string_result;
}

int c7_string_compare(C7_String *a, C7_String *b) {
  int result = 0;

  result = a->len - b->len;
  if (result) return result;

  for (int i = 0; i < a->len; ++i) {
    result = a->data[i] - b->data[i];
    if (result) return result;
  }

  return result;
}

typedef struct {
  void *return_address;
  uint32_t offset, len;
} C7_Location_Struct;

extern C7_Location_Struct __callsites_array_begin[];
extern C7_Location_Struct __callsites_array_end[];
extern const char __callsites_strings[];

C7_String *c7_backtrace_string() {
  uint64_t *frame_pointer = __builtin_frame_address(0);
  uint64_t *return_address = (uint64_t *)*(frame_pointer + 1);

  C7_String *result = (C7_String *) c7_dynamic_allocation(sizeof(C7_String));
  char *data = NULL;
  uint64_t used = 0;
  uint64_t cap = 0;

  for (;;) {
    const char fallback_location[] = "<unknown>";
    const char *location = fallback_location;
    int location_len = sizeof(fallback_location);

    for (C7_Location_Struct *it = __callsites_array_begin; it < __callsites_array_end; it++) {
      if (it->return_address == return_address) {
        location =  __callsites_strings + it->offset;
        location_len = it->len;
        break;
      }
    }


    int needed_len = stbsp_snprintf(NULL, 0, "0x%p %.*s\n", return_address, location_len, location) + 1;
    if (used + needed_len >= cap) {
      uint64_t new_cap = cap * 2;
      if (!new_cap) new_cap = 32;
      while (used + needed_len >= new_cap) new_cap *= 2;

      char *new_data = c7_dynamic_allocation(new_cap);

      if (cap) {
        for (uint64_t i = 0; i < used; ++i) {
          new_data[i] = data[i];
        }
      }

      data = new_data;
      cap = new_cap;
    }


    used += stbsp_snprintf(data + used, cap - used, "0x%p %.*s\n", return_address, location_len, location);

    frame_pointer = (uint64_t *) *frame_pointer;
    if (!frame_pointer) break;
    return_address = (uint64_t *)*(frame_pointer + 1);
  }


  result->data = data;
  result->len = used;

  return result;
}
