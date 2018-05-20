#!/bin/sh
su root -c 'cd /usr/pkgsrc/devel/cppcheck && sudo make install PKG_OPTIONS.python27=-x11' &&
    su root -c 'cd /usr/pkgsrc/devel/splint && sudo make install' &&
    su root -c 'cd /usr/pkgsrc/devel/vera++ && sudo make install'
