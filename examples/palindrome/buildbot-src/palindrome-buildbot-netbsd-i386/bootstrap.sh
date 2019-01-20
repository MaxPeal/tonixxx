#!/bin/sh
su root -c 'cd /usr/pkgsrc && cvs update -dP' &&
    su root -c 'cd /usr/pkgsrc/devel/cppcheck && make install clean clean-depends PKG_OPTIONS.python27=-x11' &&
    su root -c 'cd /usr/pkgsrc/devel/vera++ && make install clean clean-depends' &&
    cp -r /usr/pkg/share/vera++ /home/vagrant/.vera++ &&
    ln -sf /vagrant/vera /home/vagrant/.vera++/profiles/default &&
    su root -c 'cd /usr/pkgsrc/lang/python36 && make install clean clean-depends PKG_OPTIONS.python36=-x11' &&
    su root -c 'cd /usr/pkgsrc/devel/py-pip && make install clean clean-depends PYTHON_VERSION_REQD=36 PKG_OPTIONS.python36=-x11'
    su root -c 'cd /usr/pkgsrc/security/mozilla-rootcerts && make install clean clean-depends && mozilla-rootcerts install' &&
    su root -c 'pip3.6 install cpplint'
