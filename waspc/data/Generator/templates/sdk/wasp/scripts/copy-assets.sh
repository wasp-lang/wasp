#!/usr/bin/env bash

set -euo pipefail # Bash "strict mode" http://redsymbol.net/articles/unofficial-bash-strict-mode/
shopt -s nullglob # Return nothing if no files match the glob

base=$(realpath "$(dirname "$0")/..")

globs=(
  "$base"/auth/forms/*.css
  "$base"/auth/forms/internal/*.css
  "$base"/auth/forms/internal/*/*.css
)

for src_file in "${globs[@]}"; do
  relative_path="${src_file#"$base"/}"
  dest_file="$base/dist/$relative_path"

  dest_dir="$(dirname "$dest_file")"
  mkdir -p "$dest_dir"
  cp "$src_file" "$dest_file"
done
