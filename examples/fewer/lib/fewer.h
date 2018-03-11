// Copyright 2017 Andrew Pennebaker

#pragma once

// Format a byte as a hexadecimal string
void render_boi(char b, /*@out@*/ char* s);

// Parse a hexadecimal string to a byte
unsigned char parse_boi(char* s);
