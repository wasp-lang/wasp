function slugify {
  # Convert to lowercase, replace spaces and non-alphanumeric with hyphens
  echo "$1" \
    | tr '[:upper:]' '[:lower:]' \
    | tr ' ' '-' \
    | tr -cd '[:alnum:]-'
}

function splitToJsonArray {
  splitChar=$1
  inputString=$2

  echo "$inputString" \
    | jq -Rs \
      --arg splitChar "$splitChar" \
      '
        gsub("^\\s+"; "")
        | gsub("\\s+$"; "")
        | split($splitChar)
        | map(
            gsub("^\\s+"; "")
            | gsub("\\s+$"; "")
          )
        | map(select(length > 0))
      '
}

function log {
  echo "[LOG] $1" >&2
}
