#!/bin/sh
set -euf
unset IFS

mkdir docs
cd docs

HOST_PATH="github.com/${TRAVIS_REPO_SLUG}.git"

git clone -b gh-pages "https://${GH_REPO_TOKEN}@${HOST_PATH}"
cd "${TRAVIS_REPO_SLUG#*/}"

git config --global push.default simple
git config user.name "TravisCI"
git config user.email "travis@travis-ci.org"

rm -rf *

find . -name Doxyfile -exec doxygen "{}" \;

git add --all
git commit -m 'doxygen'
git push -f "https://${GH_REPO_TOKEN}@${HOST_PATH}" 2>&1 >/dev/null
