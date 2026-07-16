#!/usr/bin/env bash

set -euxo pipefail

current_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
published_packages_list_file="$current_dir/published-packages.txt"

override_version="$1"
if [ -z "$override_version" ]; then
	echo "Error: override_version is not provided."
	exit 1
fi

output_dir="$2"
if [ -z "$output_dir" ]; then
	echo "Error: output_dir is not provided."
	exit 1
fi

cat "$published_packages_list_file" | while IFS= read -r package; do
	echo "Building $package"

	pack_dir=$(mktemp -d)
	package_output_dir="$output_dir/$(basename "$package")"

	cd "$current_dir/$package"

	npm ci

	# Override the package version with the one from the workflow.
	npm pkg set version="$override_version"

	# `npm pack` puts only the published files in the tarball.
	npm run build
	npm pack --pack-destination "$pack_dir"

	# We extract the tarball back into a folder, because `pkg-pr-new` only accepts
	# package directories, not tarballs.
	mkdir -p "$package_output_dir"
	tar -xzf "$pack_dir"/*.tgz -C "$package_output_dir" --strip-components=1
done
