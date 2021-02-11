#!/bin/sh -e

# NOTE: Heavily inspired by get-stack.hs script for installing stack.
# https://raw.githubusercontent.com/commercialhaskell/stack/stable/etc/scripts/get-stack.sh

HOME_LOCAL_BIN="$HOME/.local/bin"
HOME_LOCAL_SHARE="$HOME/.local/share"
WASP_TEMP_DIR=
FORCE=

RED="\033[31m"
GREEN="\033[32m"
BOLD="\033[1m"
RESET="\033[0m"

while [ $# -gt 0 ]; do
    case "$1" in
        -f|--force)
            FORCE="true"
            shift
            ;;
        # -d|--dest)
        #     DEST="$2"
        #     shift 2
        #     ;;
        *)
            echo "Invalid argument: $1" >&2
            exit 1
            ;;
    esac
done

main() {
    trap cleanup_temp_dir EXIT
    send_telemetry > /dev/null 2>&1 &
    install_based_on_os
}

install_based_on_os() {
    case "$(uname)" in
        "Linux")
            install_from_bin_package "wasp-linux-x86_64.tar.gz"
            ;;
        "Darwin")
            install_from_bin_package "wasp-osx-x86_64.tar.gz"
            ;;
        *)
            die "Sorry, this installer does not support your operating system: $(uname)."
    esac
}

get_os_info() {
    case "$(uname)" in
        "Linux")
            echo "linux"
            ;;
        "Darwin")
            echo "osx"
            ;;
        *)
            echo "Unknown"
            ;;
    esac
}

# TODO: Add option to specify which release to download.

# Download a Wasp binary package and install it in $HOME_LOCAL_BIN.
install_from_bin_package() {
    PACKAGE_URL="https://github.com/wasp-lang/wasp/releases/latest/download/$1"
    make_temp_dir
    info "Downloading binary package to temporary dir and unpacking it there...\n"
    dl_to_file "$PACKAGE_URL" "$WASP_TEMP_DIR/$1"
    echo ""
    mkdir -p "$WASP_TEMP_DIR/wasp"
    if ! tar xzf "$WASP_TEMP_DIR/$1" -C "$WASP_TEMP_DIR/wasp"; then
      die "Unpacking binary package failed."
    fi

    # TODO: Consider installing into /usr/local/bin and /usr/local/share instead,
    #   since those are always on the PATH and are standard place to install programs like this.
    #   But then we need to run some commands below with sudo.
    DATA_DST_DIR="$HOME_LOCAL_SHARE"
    create_dir_if_missing "$DATA_DST_DIR"
    BIN_DST_DIR="$HOME_LOCAL_BIN"
    create_dir_if_missing "$BIN_DST_DIR"

    # If our install locations are already occupied (by previous wasp installation or smth else),
    # inform user that they have to clean it up (or if FORCE is set, we do it for them).

    OCCUPIED_PATH_ERRORS=""
    if [ -e "$DATA_DST_DIR/wasp" ]; then
        if [ "$FORCE" = "true" ]; then
            info "Removing already existing $DATA_DST_DIR/wasp."
            rm -r "$DATA_DST_DIR/wasp"
        else
            OCCUPIED_PATH_ERRORS=$OCCUPIED_PATH_ERRORS"Directory $DATA_DST_DIR/wasp already exists.\n"
        fi
    fi
    if [ -e "$BIN_DST_DIR/wasp" ]; then
        if [ "$FORCE" = "true" ]; then
            info "Writing over existing $BIN_DST_DIR/wasp."
        else
            OCCUPIED_PATH_ERRORS=$OCCUPIED_PATH_ERRORS"Binary file $BIN_DST_DIR/wasp already exists.\n"
        fi
    fi
    if [ ! -z "$OCCUPIED_PATH_ERRORS" ]; then
        die "\nInstallation failed!\n\n${OCCUPIED_PATH_ERRORS}\nRemove listed entries manually or run the installer with --force flag to write over them:\n\n  ${BOLD}curl -sSL http://get.wasp-lang.dev | sh -s -- --force${RESET}\n"
    fi

    info "Installing Wasp data to $DATA_DST_DIR/wasp."
    if ! mv "$WASP_TEMP_DIR/wasp" "$DATA_DST_DIR/"; then
        die "Installing data to $DATA_DST_DIR failed."
    fi

    info "Installing Wasp executable to $BIN_DST_DIR/wasp."
    # TODO: I should make sure here that $DATA_DST_DIR is abs path.
    #  It works for now because we set it to HOME_LOCAL_SHARE which
    #  we obtained using $HOME which is absolute, but if that changes
    #  and it is not absolute any more, .sh file generated below
    #  will not work properly.
    printf '#!/usr/bin/env bash\nwaspc_datadir=%s/wasp/data %s/wasp/wasp-bin "$@"\n' "$DATA_DST_DIR" "$DATA_DST_DIR" \
           > "$BIN_DST_DIR/wasp"
    if ! chmod +x "$BIN_DST_DIR/wasp"; then
        die "Failed to make $BIN_DST_DIR/wasp executable."
    fi

    if ! on_path "$BIN_DST_DIR"; then
        info "\n${RED}WARNING${RESET}: It looks like '$BIN_DST_DIR' is not on your PATH! You will not be able to invoke wasp from the terminal by its name."
        info "  You can add it to your PATH by adding following line into your profile file (~/.profile or ~/.zshrc or ~/.bash_profile or ~/.bashrc or some other, depending on your configuration):"
        info "      ${BOLD}"'export PATH=$PATH:'"$BIN_DST_DIR${RESET}"
    fi

    info "\n${GREEN}Wasp has been successfully installed! To create your first app, do:${RESET}"
    if ! on_path "$BIN_DST_DIR"; then
        info " - Add wasp to your PATH as described above."
    fi
    info " - ${BOLD}wasp new MyApp${RESET}\n"
}

create_dir_if_missing() {
    if [ ! -d "$1" ]; then
        info "$1 does not exist, creating it..."
        if ! mkdir -p "$1" 2>/dev/null; then
            die "Could not create directory: $1."
        fi
    fi
}

# Creates a temporary directory, which will be cleaned up automatically
# when the script finishes
make_temp_dir() {
    WASP_TEMP_DIR="$(mktemp -d 2>/dev/null || mktemp -d -t wasp)"
}

# Cleanup the temporary directory if it's been created.
# Called automatically when the script exits.
cleanup_temp_dir() {
    if [ -n "$WASP_TEMP_DIR" ] ; then
        rm -rf "$WASP_TEMP_DIR"
        WASP_TEMP_DIR=""
    fi
}

# Print a message to stderr and exit with error code.
die() {
    printf "${RED}$@${RESET}\n" >&2
    exit 1
}

info() {
    printf "$@\n"
}

# Download a URL to file using 'curl' or 'wget'.
dl_to_file() {
    if has_curl ; then
        if ! curl ${QUIET:+-sS} --fail -L -o "$2" "$1"; then
            die "curl download failed: $1"
        fi
    elif has_wget ; then
        if ! wget ${QUIET:+-q} "-O$2" "$1"; then
            die "wget download failed: $1"
        fi
    else
        die "Neither wget nor curl is available, please install one to continue."
    fi
}

# Check whether 'wget' command exists.
has_wget() {
    has_cmd wget
}

# Check whether 'curl' command exists.
has_curl() {
    has_cmd curl
}

# Check whether the given command exists.
has_cmd() {
    command -v "$1" > /dev/null 2>&1
}

# Check whether the given (query) path is listed in the PATH environment variable.
on_path() {
    # Below we normalize PATH and query regarding ~ by ensuring ~ is expanded to $HOME, avoiding
    # false negatives in case where ~ is expanded in query but not in PATH and vice versa.

    # NOTE: If $PATH or $1 have '|' somewhere in it, sed commands bellow will fail due to using | as their delimiter.

    # If ~ is after : or if it is the first character in the path, replace it with expanded $HOME.
    # For example, if $PATH is ~/martin/bin:~/martin/~tmp/bin,
    # result will be /home/martin/bin:/home/martin/~tmp/bin .
    local PATH_NORMALIZED=$(printf '%s' "$PATH" | sed -e "s|:~|:$HOME|g" | sed -e "s|^~|$HOME|")

    # Replace ~ with expanded $HOME if it is the first character in the query path.
    local QUERY_NORMALIZED=$(printf '%s' "$1" | sed -e "s|^~|$HOME|")

    echo ":$PATH_NORMALIZED:" | grep -q ":$QUERY_NORMALIZED:"
}

send_telemetry() {
    DATA='{ "api_key": "CdDd2A0jKTI2vFAsrI9JWm3MqpOcgHz1bMyogAcwsE4", "type": "capture", "event": "install-script:run", "distinct_id": "'$RANDOM`date +'%s%N'`'", "properties": { "os": "'`get_os_info`'" } }'
    URL="https://app.posthog.com/capture"
    HEADER="Content-Type: application/json"

    if [ -z "$WASP_TELEMETRY_DISABLE" ]; then
        if has_curl; then
            curl -sfL -d "$DATA" --header "$HEADER" "$URL" > /dev/null 2>&1
        elif has_wget; then
            wget -q --post-data="$DATA" --header="$HEADER" "$URL" > /dev/null 2>&1
        fi
    fi
}

main
