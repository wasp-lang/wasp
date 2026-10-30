#!/usr/bin/env bash

set -euxo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
packages_dir="$repo_root/packages"

# `wasp` depends on `cli` and `spec` with `workspace:*`, which `pnpm pack`
# replaces with the versions in their package.json files, so we set the version
# on `cli` too even though this script does not pack it (make-npm-packages does).
versioned_packages=(
  cli
  spec
  wasp
)
publish_packages=(
  spec
  wasp
)

publish_version="$1"
if [ -z "$publish_version" ]; then
  echo "Error: publish_version is not provided."
  exit 1
fi

output_dir="$2"
if [ -z "$output_dir" ]; then
  echo "Error: output_dir is not provided."
  exit 1
fi

(cd "$repo_root" && pnpm install)

# Override the package versions with the one from the workflow.
for package in "${versioned_packages[@]}"; do
  (cd "$packages_dir/$package" && npm pkg set version="$publish_version")
done

for package in "${publish_packages[@]}"; do
  echo "Building $package"

  pack_dir=$(mktemp -d)
  package_output_dir="$output_dir/$(basename "$package")"

  cd "$packages_dir/$package"

  # `pnpm pack` puts only the published files in the tarball.
  pnpm run --if-present build
  pnpm pack --pack-destination "$pack_dir"

  # We extract the tarball back into a folder, because `pkg-pr-new` only accepts
  # package directories, not tarballs.
  mkdir -p "$package_output_dir"
  tar -xzf "$pack_dir"/*.tgz -C "$package_output_dir" --strip-components=1
done
