#!/bin/sh
set -euf
unset IFS

DOCS='docs'
mkdir "$DOCS"

HOST_PATH="github.com/${TRAVIS_REPO_SLUG}.git"
REPO="${TRAVIS_REPO_SLUG#*/}"

sh -c "cd \"\$DOCS\" && git clone -b gh-pages \"https://\${GH_REPO_TOKEN}@\${HOST_PATH}\" && cd \"\$REPO\" && rm -rf *"

DOXYGEN_FILES="$(find . -name Doxyfile)"

for DOXYGEN_FILE in $DOXYGEN_FILES; do
    doxygen "$DOXYGEN_FILE"

    PROJECT="$(dirname -- "$DOXYGEN_FILE")"

    mkdir -p "${DOCS}/${REPO}/${PROJECT}"
    cp -r "${PROJECT}/html" "${DOCS}/${REPO}/${PROJECT}"
done

git config --global push.default simple
git config user.name "TravisCI"
git config user.email "travis@travis-ci.org"
git add --force --all
git commit -m 'doxygen'
git push -f "https://${GH_REPO_TOKEN}@${HOST_PATH}" 2>&1 >/dev/null
