#pragma once

// Copyright 2017 Andrew Pennebaker

// Format a byte as a hexadecimal string
void render_boi(char b, /*@out@*/ char *s);

// Parse a hexadecimal string to a byte
unsigned char parse_boi(char *s);

// Present a help menu.
void show_commands(int console_out);

// Removes any trailing CR/LF/CRLF.
void chomp(char *s);

// Present an interactive command session.
// Returns a success/failure exit code.
int repl(
    int console_out,
    int console_err,
    int console_in,
    int root,
    /*@out@*/ char *instruction,
    size_t instruction_size,
    /*@out@*/ char *buffer,
    /*@out*/ char *hex_buf,
    size_t hex_buf_size
);
