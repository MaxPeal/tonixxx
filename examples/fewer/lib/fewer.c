#include <stdlib.h>
#include <stdio.h>
#include "fewer.h"

// Format a byte as a hexadecimal string
void render_boi(byte b, char* s) {
  sprintf(s, "%02x", b);
}

// Parse a hexadecimal string to a byte
byte parse_boi(char* s) {
  return (byte) strtol(s);
}
