#include <stdbool.h>
#include <stdio.h>
#include "fewer.h"

const char* PROMPT = "> ";

void usage(char* program) {
  printf("Usage: %s <filename>\n");
  exit(1);
}

int repl(FILE* file, char* instruction, byte buffer[]) {
  size_t read_count;

  while (true) {
    printf("%s", PROMPT);
    scanf("%1023s", instruction);

    switch (instruction[0]) {
      case 'n':
        read_count = fread(&buffer, 1, 1, file);

        if (read_count != 1) {
          printf("Error reading byte");
          return 1;
        }

        printf("%s\n", render_boi(buffer[0]));
        break;
      case 'q':
        return 0;
    }
  }
}

int main(int argc, char** argv) {
  int status;
  FILE* file;
  byte buffer[1];
  char* filename;
  char* instruction;
  instruction = malloc(1024 * sizeof(instruction));

  if (argc != 2) {
    usage(argv[0]);
  }

  filename = argv[1];

  file = fopen(filename, "rb");

  if (file == NULL) {
    printf("Error reading %s\n", filename);
    exit(1);
  }

  status = repl(file, instruction, buffer);

  if (!fclose(file)) {
    printf("Error closing %s\n", filename);
    exit(1);
  }

  free(instruction);

  exit(status);
}
