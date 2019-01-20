#!/bin/sh
IFS=$'\n\t'

SYS="$(uname -a)"

xargs_vera() {
    case "$SYS" in
        *NetBSD*)
            xargs -0 -n1 vera++
            ;;
        *)
            xargs -0 -n1 vera++ -se
            ;;
    esac
}

/usr/bin/find lib \
    -type f \
    \( \
        -iname '*.[ch]pp' -o \
        -iname '*.cc' -o \
        -iname '*.hh' -o \
        -iname '*.[ch]xx' -o \
        -iname '*.[ch]++' -o \
        -iname '*.[ch]' \
    \) \
    -print0 |
    xargs_vera
