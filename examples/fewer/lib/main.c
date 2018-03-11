// Copyright 2017 Andrew Pennebaker

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include "fewer.h"
#include "main.h"

static const char* PROMPT = "> ";

void usage(char* program) {
  printf("Usage: %s -t|-h|<filename>\n", program);
  printf("-t\tRun self-test\n");
  printf("-h\tShow usage information\n");
}

void repl(FILE* file, /*@out@*/ char* instruction, /*@out@*/ char* buffer) {
  size_t read_count;
  int item_count;
  unsigned char c;
  char* hex_buf;

  hex_buf = malloc(3 * sizeof(char));
  assert(hex_buf != NULL);

  while (true) {
    printf("%s", PROMPT);
    item_count = scanf("%1023s", instruction);

    if (item_count == EOF) {
      free(hex_buf);
      return;
    }

    switch (instruction[0]) {
      case 'n':
        read_count = fread(buffer, 1, 1, file);

        if (read_count != 1) {
          printf("Error reading byte");

          free(hex_buf);
          return;
        }

        render_boi(buffer[0], hex_buf);
        printf("%s\n", hex_buf);

        break;
      case 'r':
        (void) scanf("%02s", hex_buf);
        c = parse_boi(hex_buf);
        printf("%c\n", c);

        break;
      case 'q':
        free(hex_buf);
        return;
    }
  }
}

int main(int argc, char** argv) {
  FILE* file;
  char* filename;
  char* buffer;
  char* instruction;

  buffer = malloc(1 * sizeof(char));
  assert(buffer != NULL);

  instruction = malloc(1024 * sizeof(char));
  assert(instruction != NULL);

  if (argc != 2) {
    usage(argv[0]);
    return EXIT_FAILURE;
  }

  if (strcmp(argv[1], "-h") == 0) {
    usage(argv[0]);
    return EXIT_SUCCESS;
  } else if (strcmp(argv[1], "-t") == 0) {
    char* hex_buf;

    hex_buf = malloc(3 * sizeof(char));
    assert(hex_buf != NULL);

    char c;

    for (int i = 0; i <= CHAR_MAX; i++) {
      c = (char) i;

      render_boi(c, hex_buf);

      unsigned char d = parse_boi(hex_buf);

      if (d != c) {
        printf("Character %c corrupted to %c during hexadecimal translation\n", c, d);
        return EXIT_FAILURE;
      }
    }
  } else {
    filename = argv[1];

    file = fopen(filename, "rb");

    if (file == NULL) {
      printf("Error reading %s\n", filename);
      return EXIT_FAILURE;
    }

    repl(file, instruction, buffer);

    (void) fclose(file);
    free(buffer);
  }

  return EXIT_SUCCESS;
}
