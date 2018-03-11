// Copyright 2017 Andrew Pennebaker

#include <stdlib.h>
#include <stdio.h>
#include "fewer.h"

// Format a byte as a hexadecimal string
void render_boi(char b, char* s) {
  sprintf(s, "%02x", b);
}

// Parse a hexadecimal string to a byte
unsigned char parse_boi(char* s) {
  return (unsigned char) strtol(s, NULL, 16);
}
