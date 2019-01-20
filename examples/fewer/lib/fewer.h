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
    #elif defined(PATH_MAX)
        #define _XOPEN_PATH_MAX PATH_MAX
    #else
        #define _XOPEN_PATH_MAX 1024
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
// Returns EOF and sets errno on error.
int show_commands(FILE *console);

// Format a byte as a hexadecimal string.
// Any errors during processing are emitted to console.
void render_boi(FILE *console, unsigned int b, /*@out@*/ char *s, size_t length);

// Parse a hexadecimal string to a byte.
// Returns -1 on range error and sets errno.
// Returns 0 on parse error.
short parse_boi(char *s);

// Removes any trailing CR/LF/CRLF.
void chomp(char *s, size_t length);

// Entrypoint parameters
typedef struct {
    /*@dependent@*/ FILE *console_err;
    /*@dependent@*/ FILE *console_out;
    /*@dependent@*/ FILE *console_in;
    int root;
    bool test;
} fewer_config;

// Check for basic errors in a fewer_config.
// May set errno on file errors.
bool validate_fewer_config(fewer_config *config);

// Run unit tests.
// Returns a system exit status.
int unit_test(fewer_config *config);

// Present an interactive command session.
// Returns a system exit status.
int repl(fewer_config *config);
