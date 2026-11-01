#!/usr/bin/env bash

set -euxo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
packages_dir="$repo_root/packages"

# Paths are relative to `packages/`. Append `:<name>` to publish a package
# under a different name than the one in its `package.json`.
publish_packages=(
  spec
  libs/test-utils:@wasp/test-utils
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
  package_path="${package%%:*}"
  publish_name=""
  if [[ "$package" == *:* ]]; then
    publish_name="${package#*:}"
  fi

  echo "Building $package_path"

  pack_dir=$(mktemp -d)
  package_output_dir="$output_dir/$(basename "$package_path")"

  # Libs use other libs' types (e.g. `@wasp/lib-sdk-core`), so we also build
  # the workspace packages the package depends on.
  (cd "$repo_root" && pnpm --filter "./packages/$package_path..." run build)

  cd "$packages_dir/$package_path"

  # Override the package version with the one from the workflow.
  npm pkg set version="$publish_version"

  if [ -n "$publish_name" ]; then
    npm pkg set name="$publish_name"
  fi

  # `pnpm pack` puts only the published files in the tarball.
  pnpm pack --pack-destination "$pack_dir"

  # We extract the tarball back into a folder, because `pkg-pr-new` only accepts
  # package directories, not tarballs.
  mkdir -p "$package_output_dir"
  tar -xzf "$pack_dir"/*.tgz -C "$package_output_dir" --strip-components=1
done
