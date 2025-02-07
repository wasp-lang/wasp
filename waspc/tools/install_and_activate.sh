#!/bin/bash -e

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
HOME_DIR=$HOME
WASP_LANG_DIR=$HOME_DIR/.local/share/wasp-lang
ACTIVE_FILE=$WASP_LANG_DIR/active
BIN_DIR=$HOME_DIR/.local/bin
CABAL_ALIAS=$HOME_DIR/.cabal/bin/wasp-cli
FORCE_INSTALL=false

for arg in "$@"; do
  if [[ "$arg" == "--force" ]]; then
    FORCE_INSTALL=true
  fi
done

# get version from waspc.cabal file
WASP_VERSION=$(awk '/^version:/ {print $2}' "$SCRIPT_DIR/../waspc.cabal")
if [[ -z "$WASP_VERSION" ]]; then
  echo "Error: Unable to extract version from waspc.cabal"
  exit 1
fi

TARGET_DIR="$WASP_LANG_DIR/$WASP_VERSION"
FINAL_WASP_VERSION="$WASP_VERSION"

if [[ -d "$TARGET_DIR" ]]; then
  echo "A version directory already exists: $TARGET_DIR"
  echo "You can add a suffix to the version or overwrite the existing version."

  while true; do
    read -p "Enter a suffix (or press Enter to overwrite): " SUFFIX
    CLEANED_SUFFIX=$(echo "$SUFFIX" | sed 's/[^a-zA-Z0-9-]//g')
    CLEANED_SUFFIX=$(echo "$CLEANED_SUFFIX" | sed 's/^-*//')

    if [[ -z "$SUFFIX" ]]; then
      break
    elif [[ "$CLEANED_SUFFIX" == "$SUFFIX" ]]; then
      break 
    else
      echo "Invalid suffix. Only letters, numbers, and dashes are allowed, and dashes cannot be at the beginning."
    fi
  done

  if [[ -n "$CLEANED_SUFFIX" ]]; then
    TARGET_DIR="$WASP_LANG_DIR/${WASP_VERSION}-${CLEANED_SUFFIX}"
    FINAL_WASP_VERSION="${WASP_VERSION}-${CLEANED_SUFFIX}"
  fi
fi

"$SCRIPT_DIR/install_packages_to_data_dir.sh"

cabal install --overwrite-policy=always

echo "Post-install: Setting up your Wasp build..."

if [[ ! -f "$CABAL_ALIAS" ]]; then
  echo "Error: Alias $CABAL_ALIAS does not exist."
  exit 1
fi

if [[ "$FORCE_INSTALL" == true ]]; then
  echo "Overwriting existing version..."
  chmod -R u+w "$TARGET_DIR" || sudo chmod -R u+w "$TARGET_DIR"
  rm -rf "$TARGET_DIR"
fi

REAL_PATH=$(realpath "$CABAL_ALIAS")
STORE_DIR=$(dirname "$(dirname "$REAL_PATH")")
SHARE_DIR="$STORE_DIR/share"

mkdir -p "$TARGET_DIR/data"
cp "$REAL_PATH" "$TARGET_DIR/wasp-bin"
chmod +x "$TARGET_DIR/wasp-bin"
cp -r "$SHARE_DIR/" "$TARGET_DIR/data/"
echo -n "$FINAL_WASP_VERSION" | sed 's/[[:space:]]//g' > "$ACTIVE_FILE"
mkdir -p "$BIN_DIR"
WASP_BIN="$BIN_DIR/wasp"

# Create alias script at ~/.local/bin/wasp if it does not exist OR if --force is enabled
if [[ ! -f "$WASP_BIN" || "$FORCE_INSTALL" == true ]]; then
  echo "Creating alias script at $WASP_BIN"
  echo -e "#!/usr/bin/env bash\nwaspc_datadir=\"$TARGET_DIR/data\" \"$TARGET_DIR/wasp-bin\" \"\$@\"" > "$WASP_BIN"
  chmod +x "$WASP_BIN"
  RELEASE_FILE="$WASP_LANG_DIR/release"
  echo -n "$FINAL_WASP_VERSION" | tr -d '[:space:]' > "$RELEASE_FILE"
  echo "Updated release version to $FINAL_WASP_VERSION"
fi

echo "Wasp $FINAL_WASP_VERSION installed successfully and set to the active version!"
echo "You can run it using: wasp <command>"
