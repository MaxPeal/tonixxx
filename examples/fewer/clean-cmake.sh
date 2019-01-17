#!/bin/sh
/usr/bin/find . \
    -iwholename '*cmake*' \
    -not -name CMakeLists.txt \
    -delete
