set -euo pipefail

packageName=$1
inputDataJson=$2
subPackageNamesJson=$3
outputDirPath=$4

templateDir="$(dirname "$0")/template"

source "$(dirname "$0")/../util.bash"

rm -rf "${outputDirPath:?}/*" || true
mkdir -p "$outputDirPath"

cp "$templateDir/bin.js" "$outputDirPath/bin.js"

jq < "$templateDir/package.json" \
  --arg packageName "$packageName" \
  --argjson subPackageNames "$subPackageNamesJson" \
  --argjson inputData "$inputDataJson" \
  '
    .
    | .name = $packageName
    | .version = $inputData.version
    | .optionalDependencies = (
        $subPackageNames
        | map({ (.): $inputData.version })
        | add
      )
    | .os = ($inputData.tarballs | map(.target[0]) | unique)
    | .cpu = ($inputData.tarballs | map(.target[1]) | unique)
  ' \
  > "$outputDirPath/package.json"

jq -n \
  --argjson subPackageNames "$subPackageNamesJson" \
  '{ subPackageNames: $subPackageNames }' \
  > "$outputDirPath/data.json"

echo "$outputDirPath"
