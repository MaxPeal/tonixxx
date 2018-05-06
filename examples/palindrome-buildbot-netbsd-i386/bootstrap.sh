#!/bin/sh
su root -c 'cd /usr/pkgsrc/devel/cppcheck && make install PKG_OPTIONS.python27=-x11' &&
    su root -c 'cd /usr/pkgsrc/devel/splint && make install' &&
    su root -c 'cd /usr/pkgsrc/devel/vera++ && make install'
