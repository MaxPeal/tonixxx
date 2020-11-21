/**
 * @copyright 2020 YelloSoft
 */

#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#if defined(_MSC_VER)
#include <direct.h>
#include <io.h>
#else
#include <unistd.h>
#endif

#if defined(_MSC_VER) || defined(__MirBSD__) || defined(__minix)
#include <stdarg.h>
#endif

#include "fewer/fewer.h"

#if defined(_MSC_VER)
int fchdir(int fd) {
    DWORD result;
    char base_path[X_PATH_MAX];

    result = GetFinalPathNameByHandleA(
        (HANDLE) fd,
        base_path,
        sizeof(base_path)/sizeof(base_path[0]) - 1,
        FILE_NAME_NORMALIZED
    );

    if (result > sizeof(base_path)/sizeof(base_path[0])) {
        return -1;
    }

    if (result == 0) {
        return -1;
    }

    errno = 0;
    fd = _chdir(base_path);
    return fd;
}
#endif

#if defined(_MSC_VER) || defined(__MirBSD__) || defined(__minix)
int openat(int fd, const char *path, int flags, ...) {
    mode_t mode = 0;

    if (flags & O_CREAT) {
        va_list arg;
        va_start(arg, flags);
        mode = va_arg(arg, mode_t);
        va_end(arg);
    }

    errno = 0;
    if (fchdir(fd) != 0) {
        return -1;
    }

    errno = 0;
#if defined(_MSC_VER)
    if (_sopen_s(&fd, path, flags, _SH_DENYNO, mode) != 0) {
        return -1;
    }
#else
    fd = open(path, flags, mode);
#endif

    return fd;
}
#endif

void render_boi(FILE *console, unsigned int b, /*@out@*/ char *s, size_t s_len) {
    int write_count = snprintf(s, s_len, "%02x", b);

    if (write_count < 0) {
        fprintf(console, "Encoding error\n");
    } else if ((size_t) write_count > s_len - 1) {
        fprintf(console, "Buffer requires %d character allocation to render value\n", write_count);
    }
}

short parse_boi(char *s) {
    char *endptr = s;
    unsigned long int n = strtoul(s, &endptr, 16);

    if (endptr == s) {
        return -1;
    }

    if (n == ULONG_MAX) {
        return -1;
    }

    if (n > UINT_MAX) {
        return -1;
    }

    return (short) n;
}
