// Copyright 2017 Andrew Pennebaker

#pragma once

// Display command line syntax information.
void usage(char* program) __attribute((noreturn));

// Present an interactive command session.
void repl(FILE* file, char* instruction, char* buffer);
