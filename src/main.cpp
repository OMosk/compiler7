#include "compiler.hpp"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/mman.h>

int main(int argc, char **argv) {
  Compilation_Params params = {};
  params.backend = BACKEND_X64;

  int arg_to_use_for_input = 1;
  for (int i = 1; i < argc; ++i) {
    if (strcmp(argv[i], "--nasm-backend") == 0) {
      params.backend = BACKEND_NASM_LD; 
      arg_to_use_for_input = i + 1;
    } else if (strcmp(argv[i], "--x64-backend") == 0) {
      params.backend = BACKEND_X64; 
      arg_to_use_for_input = i + 1;
    } else if (argv[i][0] == '-' && argv[i][1] == '-') {
      printf("Unknown option %s\n", argv[i]);
      return 1;
    }
  }

  if (arg_to_use_for_input == argc) {
    printf("Provide input file to compile\n");
    return 1;
  }

  const char *input = argv[arg_to_use_for_input];

  {
    int64_t memory_size = 1024 * 1024 * 1024;
    int prot = PROT_READ | PROT_WRITE;
    int flags = MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED_NOREPLACE;
    void *fixed_position = (void *)0x12345789000;

    char *memory = (char *)mmap(fixed_position, memory_size, prot, flags, 0, 0);
    if (memory == MAP_FAILED) {
      printf("%s:%d Failed to mmap memory because: %s\n",
             __FILE__, __LINE__, strerror(errno));
      abort();
    }

    thread_local_allocator = {.start = memory, .available = memory_size};
  }

  printf("Processing %s\n", input);
  params.entry_file_name = input;
  start_compilation(params);

  printf("Memory used: %ld bytes in %ld allocations\n",
         thread_local_allocator.used,
         thread_local_allocator.allocations);

  return 0;
}
