set -euo pipefail

packageName=$1
packageVersion=$2
tarballPath=$3
tarballDataJson=$4
outputDirPath=$5

templateDir="$(dirname "$0")/template"

source "$(dirname "$0")/../util.bash"

rm -rf "${outputDirPath:?}/*" || true
mkdir -p "$outputDirPath"

cp "$templateDir/main.js" "$outputDirPath/main.js"

jq < "$templateDir/package.json" \
  --arg packageName "$packageName" \
  --arg packageVersion "$packageVersion" \
  --argjson tarballData "$tarballDataJson" \
  '
    .
    | .name = $packageName
    | .version = $packageVersion
    | .os = [ $tarballData.target[0] ]
    | .cpu = [ $tarballData.target[1] ]
    | if $tarballData.target[2]
      then .libc = [ $tarballData.target[2] ]
      else .
      end
  ' \
  > "$outputDirPath/package.json"

tar -xzf "$tarballPath" -C "$outputDirPath"

echo "$outputDirPath"
