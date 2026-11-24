#!/usr/bin/env bash

set -euxo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
packages_dir="$repo_root/packages"

# The packages this script packs: `@wasp/wasp` and the `@wasp` packages it
# depends on or lists as optional peers, by their dir in `packages/`. `cli`
# isn't here, because `packages/cli/scripts/make-cli-npm-packages` packs it
# with its platform sub-packages.
publish_packages=()
while IFS= read -r package; do publish_packages+=("$package"); done < <(
  jq -r '
    [
      .name,
      (.dependencies // {} | keys[]),
      (.peerDependenciesMeta // {} | to_entries[] | select(.value.optional) | .key)
    ]
    | map(select(startswith("@wasp/") and . != "@wasp/cli") | ltrimstr("@wasp/"))
    | unique[]
  ' "$packages_dir/wasp/package.json"
)

# The packages that get the publish version, which also includes `cli`.
versioned_packages=(cli "${publish_packages[@]}")

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
  (cd "$packages_dir/$package" && pnpm pkg set version="$publish_version")
done

# pnpm builds them in dependency order, so each package finds the build output
# of its dependencies.
(cd "$repo_root" && pnpm --filter "@wasp/wasp..." run build)

for package in "${publish_packages[@]}"; do
  echo "Packing $package"

  pack_dir=$(mktemp -d)
  package_output_dir="$output_dir/$(basename "$package")"

  cd "$packages_dir/$package"

  # `pnpm pack` puts only the published files in the tarball.
  pnpm pack --pack-destination "$pack_dir"

  # We extract the tarball back into a folder, because `pkg-pr-new` only accepts
  # package directories, not tarballs.
  mkdir -p "$package_output_dir"
  tar -xzf "$pack_dir"/*.tgz -C "$package_output_dir" --strip-components=1
done
