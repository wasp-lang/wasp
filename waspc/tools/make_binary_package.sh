#!/bin/sh -e

# Takes already built binary from stack and packages it
# together with data into .tar.gz package.

DST=$(realpath ${1:-wasp.tar.gz})

TMP_DIR="$(mktemp -d 2>/dev/null || mktemp -d -t wasp-bin-package)"

cp "$(stack path --local-install-root)"/bin/wasp "$TMP_DIR/wasp-bin"
cp -R "$(stack path --project-root)/data" "$TMP_DIR/data"

cd "$TMP_DIR"
tar -czf "$DST" *

if [ -n "$TMP_DIR" ]; then rm -rf "$TMP_DIR"; fi

echo "Generated binary package: $DST."
