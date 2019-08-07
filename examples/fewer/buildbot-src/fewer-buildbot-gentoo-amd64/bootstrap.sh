#!/bin/bash
set -eEuo pipefail

echo 'app-admin/rsyslog openssl' >>/etc/portage/package.use/rsyslog

emerge -uDU --keep-going --with-bdeps=y @world
emerge \
    dev-util/cmake \
    dev-util/cppcheck \
    dev-util/splint \
    dev-util/valgrind \
    dev-lang/python \
    dev-python/pip \
    dev-python/setuptools

echo "export PATH=\"\$PATH:/home/vagrant/.local/bin\"" >>/etc/profile
emerge --depclean

sudo -u vagrant pip3 install --user wheel
sudo -u vagrant pip3 install --user cpplint

# Fix libraries for valgrind
mkdir -p /etc/portage/env
echo -e "CFLAGS=\"\${CFLAGS} -ggdb\"\nCXXFLAGS=\"\${CXXFLAGS} -ggdb\"\nFEATURES=\"\${FEATURES} splitdebug compressdebug -nostrip\"" >/etc/portage/env/debugsyms
echo -e "FEATURES=\"\${FEATURES} installsources\"" >/etc/portage/env/installsources
echo 'sys-libs/glibc debugsyms installsources' >/etc/portage/package.env
emerge --oneshot sys-libs/glibc
