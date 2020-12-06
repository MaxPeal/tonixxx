#!/bin/sh
unset IFS
set -euf

DOCS="${TRAVIS_BUILD_DIR}/docs"
PAGE_REPO="${DOCS}/_pages"

mkdir "$DOCS"

HOST_PATH="github.com/${TRAVIS_REPO_SLUG}.git"

git clone \
    -b gh-pages \
    "https://git@${HOST_PATH}" \
    "$PAGE_REPO"

cd "$PAGE_REPO"
find \
    . \
    -type f \
    -not -path '*/\.*' \
    -exec rm -rf "{}" \;
cd "$TRAVIS_BUILD_DIR"

DOXYGEN_FILES="$(find . -name Doxyfile)"

for DOXYGEN_FILE in $DOXYGEN_FILES; do
    PROJECT_BASE="$(dirname -- "$DOXYGEN_FILE")"
    PROJECT="$(readlink -f "$PROJECT_BASE")"

    cd "${PROJECT}"
    doxygen Doxyfile
    mkdir -p "${PAGE_REPO}/${PROJECT_BASE}"
    cp -r html/. "${PAGE_REPO}/${PROJECT_BASE}/"
    cd "$TRAVIS_BUILD_DIR"
done

cd "$PAGE_REPO"
git config --global push.default simple
git config user.name 'TravisCI'
git config user.email 'travis@travis-ci.org'
git add --force --all

RESULT="$(git commit -m "Travis build: ${TRAVIS_BUILD_NUMBER}" -m "Commit: ${TRAVIS_COMMIT}" || echo '')"

if [ -z "$RESULT" ]; then
    echo 'no doc changes observed'
    exit
fi

git push -f "https://${GH_REPO_TOKEN}@${HOST_PATH}" 2>&1 >/dev/null
