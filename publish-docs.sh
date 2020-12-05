#!/bin/sh
unset IFS
set -euf

DOCS="${TRAVIS_BUILD_DIR}/docs"

echo "DOCS: $DOCS"

PAGE_REPO="${DOCS}/_pages"

echo "PAGE_REPO: $PAGE_REPO"

mkdir "$DOCS"

HOST_PATH="github.com/${TRAVIS_REPO_SLUG}.git"

echo "HOST_PATH: $HOST_PATH"

git clone -b gh-pages "https://git@${HOST_PATH}" "$PAGE_REPO"

echo "CLONED PAGES"

cd "$PAGE_REPO"
rm -rf *
cd "$TRAVIS_BUILD_DIR"

DOXYGEN_FILES="$(find . -name Doxyfile)"

for DOXYGEN_FILE in $DOXYGEN_FILES; do
    PROJECT="$(readlink -f "$(dirname -- "$DOXYGEN_FILE")")"

    echo "PROJECT: $PROJECT"

    cd "${PROJECT}"

    doxygen Doxyfile

    echo "GENERATED DOCS"

    mkdir -p "${PAGE_REPO}/${PROJECT}"
    cp -r html "${PAGE_REPO}/${PROJECT}"

    echo "COPIED ARTIFACTS"
done

cd "$PAGE_REPO"
git config --global push.default simple
git config user.name 'TravisCI'
git config user.email 'travis@travis-ci.org'
git add --force --all
git commit -m 'doxygen'

echo "GENERATED COMMIT"

git push -f "https://${GH_REPO_TOKEN}@${HOST_PATH}" 2>&1 >/dev/null
