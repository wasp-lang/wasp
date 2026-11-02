#!/usr/bin/env bash

set -euxo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
packages_dir="$repo_root/packages"

publish_packages=(
  spec
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

for package in "${publish_packages[@]}"; do
  echo "Building $package"

  pack_dir=$(mktemp -d)
  package_output_dir="$output_dir/$(basename "$package")"

  cd "$packages_dir/$package"

  # Override the package version with the one from the workflow.
  npm pkg set version="$publish_version"

  # `pnpm pack` puts only the published files in the tarball.
  pnpm run build
  pnpm pack --pack-destination "$pack_dir"

  # We extract the tarball back into a folder, because `pkg-pr-new` only accepts
  # package directories, not tarballs.
  mkdir -p "$package_output_dir"
  tar -xzf "$pack_dir"/*.tgz -C "$package_output_dir" --strip-components=1
done
