#include <stdio.h>

int main(int argc, char **argv) {
  const char *name = "default";
  if (argc > 1) {
    name = argv[1];
  }

  int total_len = 0;
  int current_byte_number = 0;

  unsigned char buffer[1024];

  fprintf(stdout, "unsigned char %s_data [] = { \n", name);

  for (;;) {
    int have_read = fread(buffer, 1, sizeof(buffer), stdin);
    if (have_read > 0) {
      for (int i = 0; i < have_read; i++, current_byte_number++) {
        if (current_byte_number != 0) {
          fprintf(stdout, ",");
        }
        if (current_byte_number % 20 == 0) {
          fprintf(stdout, "\n");
        }
        fprintf(stdout, "%3d", buffer[i]);
      }
      total_len += have_read;
    } else {
      break;
    } 
  }

  fprintf(stdout, "\n};\n");
  fprintf(stdout, "unsigned long %s_len = %d;\n", name, total_len);
  return 0;
}
