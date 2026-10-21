#!/usr/bin/env bash

set -euxo pipefail

waspc_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
waspc_data_dir="$waspc_dir/data"

# Paths are relative to `waspc/data`. Append `:<name>` to publish a package
# under a different name than the one in its `package.json`.
publish_packages=(
  packages/spec
  Generator/libs/test-utils:@wasp.sh/test-utils
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

# Libs depend on other libs through local paths (e.g. `file:../sdk-core`), so
# those must be built before we build a lib for publishing.
node "$waspc_dir/tools/libs/build.ts"

for package in "${publish_packages[@]}"; do
  package_path="${package%%:*}"
  publish_name=""
  if [[ "$package" == *:* ]]; then
    publish_name="${package#*:}"
  fi

  echo "Building $package_path"

  pack_dir=$(mktemp -d)
  package_output_dir="$output_dir/$(basename "$package_path")"

  cd "$waspc_data_dir/$package_path"

  npm ci

  # Override the package version with the one from the workflow.
  npm pkg set version="$publish_version"

  if [ -n "$publish_name" ]; then
    npm pkg set name="$publish_name"
  fi

  # `npm pack` puts only the published files in the tarball.
  npm run build
  npm pack --pack-destination "$pack_dir"

  # We extract the tarball back into a folder, because `pkg-pr-new` only accepts
  # package directories, not tarballs.
  mkdir -p "$package_output_dir"
  tar -xzf "$pack_dir"/*.tgz -C "$package_output_dir" --strip-components=1
done
