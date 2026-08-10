# Makes a Nix-built macOS executable self-contained so it can run on
# machines without /nix: copies every /nix/store dylib the binary (transitively)
# links against into a libs/ dir next to it and rewrites the load commands to
# @executable_path-relative paths (the approach IOG uses for cardano-node
# releases; see haskell.nix#1239). Expected dylibs: libgmp, libffi, libz,
# libiconv — system frameworks (/usr/lib, /System) are left untouched.
#
# ad-hoc re-signing afterwards is mandatory: mutating a signed Mach-O
# invalidates its signature, and arm64 macOS refuses to run unsigned binaries.
{ pkgs }:
exe:
pkgs.runCommand "wasp-macos-bundle"
  {
    # cctools provides otool, install_name_tool, and codesign_allocate
    # (which sigtool's codesign spawns under the hood).
    nativeBuildInputs = [
      pkgs.darwin.cctools
      pkgs.darwin.sigtool
    ];
  }
  ''
    set -euo pipefail
    mkdir -p $out/libs
    cp ${exe}/bin/wasp-cli $out/wasp-bin
    chmod u+w $out/wasp-bin

    # Copies every /nix/store dylib referenced by the given Mach-O file into
    # $out/libs (recursing into the dylibs' own dependencies) and rewrites
    # the file's load commands to @executable_path/libs/<name>.
    bundle_store_dylibs() {
      local macho_file="$1"
      local dylib dylib_name
      while IFS= read -r dylib; do
        dylib_name="$(basename "$dylib")"
        if [ ! -f "$out/libs/$dylib_name" ]; then
          cp "$dylib" "$out/libs/$dylib_name"
          chmod u+w "$out/libs/$dylib_name"
          install_name_tool -id "@executable_path/libs/$dylib_name" "$out/libs/$dylib_name"
          bundle_store_dylibs "$out/libs/$dylib_name"
        fi
        install_name_tool -change "$dylib" "@executable_path/libs/$dylib_name" "$macho_file"
      done < <(otool -L "$macho_file" | tail -n +2 | awk '{print $1}' | grep '^/nix/store/' || true)
    }

    bundle_store_dylibs $out/wasp-bin

    codesign --force --sign - $out/wasp-bin
    for lib in $out/libs/*; do
      codesign --force --sign - "$lib"
    done

    # Fail loudly if any /nix/store reference survived.
    if otool -L $out/wasp-bin $out/libs/* | grep '/nix/store/'; then
      echo "ERROR: bundled macOS binary still references /nix/store" >&2
      exit 1
    fi
  ''
