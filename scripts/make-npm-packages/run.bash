#!/usr/bin/env bash

set -euo pipefail

inputDirPath=$1
outputDirPath=$2

MAIN_PACKAGE_NAME="@wasp.sh/wasp-cli"
SUB_PACKAGE_NAME_TEMPLATE="@wasp.sh/wasp-cli-%s-%s-%s" # The three arguments are OS, CPU, and LIBC

source "$(dirname "$0")/util.bash"

dataFilePath="$inputDirPath/data.json"
version=$(jq < "$dataFilePath" -cr '.version')

rm -rf "${outputDirPath:?}/*" || true
mkdir -p "$outputDirPath"

allTarballDataJsons=$(jq < "$dataFilePath" -cr '.tarballs[]')
allSubPackageNames=()

for tarballDataJson in $allTarballDataJsons; do
  readarray -t subPackageArgs < <(
    echo "$tarballDataJson" \
      | jq -cr '.target | (.[0], .[1], .[2]) | . // "unknown"'
  )
  subPackageName=$(
    # In this case, we want to have the format string be a variable in itself
    # shellcheck disable=SC2059
    printf "$SUB_PACKAGE_NAME_TEMPLATE" "${subPackageArgs[@]}"
  )
  allSubPackageNames+=("$subPackageName")

  subPackageOutputDir="$outputDirPath/$(slugify "$subPackageName")"

  tarballPath="$inputDirPath/$(echo "$tarballDataJson" | jq -cr '.fileName')"

  bash "$(dirname "$0")/sub-package/make.bash" \
    "$subPackageName" \
    "$version" \
    "$tarballPath" \
    "$tarballDataJson" \
    "$subPackageOutputDir"
done

mainPackageOutputDir="$outputDirPath/$(slugify "$MAIN_PACKAGE_NAME")"

allSubPackageNamesJson=$(splitToJsonArray " " "${allSubPackageNames[*]}")

bash "$(dirname "$0")/main-package/make.bash" \
  "$MAIN_PACKAGE_NAME" \
  "$(cat "$dataFilePath")" \
  "$allSubPackageNamesJson" \
  "$mainPackageOutputDir"
