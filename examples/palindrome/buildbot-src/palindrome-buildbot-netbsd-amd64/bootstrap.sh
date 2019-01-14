#!/bin/sh
su root -c 'cd /usr/pkgsrc && cvs update -dP' &&
    su root -c 'cd /usr/pkgsrc/devel/cppcheck && make install clean clean-depends PKG_OPTIONS.python27=-x11' &&
    su root -c 'cd /usr/pkgsrc/devel/vera++ && make install clean clean-depends'
