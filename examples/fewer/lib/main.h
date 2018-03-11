// Copyright 2017 Andrew Pennebaker

#pragma once

void usage(char* program) __attribute((noreturn));

int repl(FILE* file, char* instruction, unsigned char buffer[]);
