#pragma once

// Copyright 2017 Andrew Pennebaker

#define _GNU_SOURCE

#if defined(__sun)
    #define __EXTENSIONS__
#elif defined(_MSC_VER)
    #include <windows.h>
    typedef int mode_t;
#endif

#include <limits.h>
#include <stdbool.h>
#include <stdio.h>

#if !defined(_XOPEN_PATH_MAX)
    #if defined(_POSIX_PATH_MAX)
        #define _XOPEN_PATH_MAX _POSIX_PATH_MAX
    #elif defined(_MAX_PATH)
        #define _XOPEN_PATH_MAX _MAX_PATH
    #else
        #define _XOPEN_PATH_MAX PATH_MAX
    #endif
#endif

#if defined(_MSC_VER)
    // Change directory given a directory file descriptor.
    // Returns -1 and sets errno on failure.
    int fchdir(int fd);
#endif

#if defined(_MSC_VER) || defined(__minix)
    // Get file descriptor to file path nested in directory fd.
    // Return -1 and set errno on failure.
    int openat(int fd, const char *path, int flags, ...);
#endif

// Present a help menu.
void show_commands(FILE *console);

// Format a byte as a hexadecimal string
void render_boi(char b, /*@out@*/ char *s);

// Parse a hexadecimal string to a byte
unsigned char parse_boi(char *s);

// Removes any trailing CR/LF/CRLF.
void chomp(char *s);

// Entrypoint parameters
typedef struct {
    FILE *console_err;
    FILE *console_out;
    FILE *console_in;
    int root;
    bool test;
} fewer_config;

// Construct a fewer_config.
// By default, test is false and other parameters are unset.
//
// Returns NULL and sets errno on allocation failure.
fewer_config * new_fewer_config();

// Deallocate a fewer_config.
void destroy_fewer_config(fewer_config *config);

// Check for basic errors in a fewer_config.
bool validate_fewer_config(fewer_config *config);

// Present an interactive command session.
// Returns a system exit status.
int repl(fewer_config *config);
