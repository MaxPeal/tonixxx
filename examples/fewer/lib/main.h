// Copyright 2017 Andrew Pennebaker

#pragma once

// Display command line syntax information.
void usage(char* program);

// Present an interactive command session.
void repl(FILE* file, /*@out@*/ char* instruction, /*@out@*/ char* buffer, /*@out*/ char* hex_buf);
