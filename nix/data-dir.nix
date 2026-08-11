# The compiler's data dir (Cabal `data-files`), assembled two ways:
#
#  - `release`: byte-for-byte what `waspc/tools/make_binary_package.sh` used
#    to put in the release tarballs. The file set comes from
#    `cabal sdist --list-only` *inside the derivation*, so waspc.cabal stays
#    the single source of truth for what ships.
#  - `runtime`: `release` plus prod-only node_modules for the packages the
#    CLI executes out of the data dir. This is what the Nix-installed CLI
#    points waspc_datadir at — without the pre-populated node_modules, the
#    CLI would try to `npm install` into the read-only store on first use
#    (see ensurePackageDependenciesAreInstalled in Wasp.NodePackageFFI).
#  - `manifest`: TSV of `./data/<path>\t<sha256>` — the same format
#    ci-waspc-build.yaml exposes as `data_files_list` and
#    ci-npm-package-test.yaml diffs against (order-insensitive).
{
  pkgs,
  toolchain,
  npm,
  project,
}:
let
  inherit (pkgs) lib;

  release =
    pkgs.runCommand "wasp-data-release"
      {
        nativeBuildInputs = [
          toolchain.cabal
          toolchain.ghc
        ];
      }
      ''
        set -euo pipefail
        export HOME="$TMPDIR"

        # cabal wants a writable project dir even for --list-only.
        cp -r ${project.srcWithArtifacts} src
        chmod -R u+w src
        cd src

        cabal sdist --list-only | grep '^\./data/' > "$TMPDIR/data-files.txt"

        mkdir -p $out
        while IFS= read -r data_file; do
          mkdir -p "$out/$(dirname "$data_file")"
          cp "$data_file" "$out/$data_file"
        done < "$TMPDIR/data-files.txt"
      '';

  runtime =
    pkgs.runCommand "wasp-data-runtime" { }
      (
        ''
          cp -r ${release} $out
          chmod -R u+w $out
        ''
        + lib.concatStrings (
          lib.mapAttrsToList (name: nodeModules: ''
            cp -r --no-preserve=mode ${nodeModules}/node_modules $out/data/packages/${name}/node_modules
          '') npm.runtimeNodeModules
        )
      );

  manifest = pkgs.runCommand "wasp-data-manifest" { } ''
    cd ${release}
    find ./data -type f | sort | while IFS= read -r file; do
      hash=$(sha256sum "$file" | cut -d' ' -f1)
      printf '%s\t%s\n' "$file" "$hash"
    done > $out
  '';
in
{
  inherit release runtime manifest;
}
