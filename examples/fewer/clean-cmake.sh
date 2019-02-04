#!/bin/sh
/usr/bin/find . \
    -iwholename '*cmake*' \
    -not -name CMakeLists.txt \
    -print \
    -exec rm -rf '{}' \;
