# `nix flake check` targets. All of these are pure builds — they see only
# git-tracked sources, need no network, and are fully cacheable:
#
#  - waspc-tests / wasp-cli-tests: the Haskell unit test suites, built and
#    run by haskell.nix. (The e2e/golden suite is impure and runs via
#    `nix run .#test-waspc-e2e` instead.)
#  - ormolu / cabal-gild / prettier: formatting checks.
#  - versions-consistent: cross-file version pins that used to be enforced
#    only by comments ("keep in sync with ...").
#  - windows-unit-tests (x86_64-linux only): the unit suites cross-compiled
#    to Windows and run under wine.
{
  pkgs,
  toolchain,
  npm,
  project,
}:
let
  inherit (pkgs) lib;

  # Version-pin consistency, previously enforced by build-time asserts
  # (waspc/tools/utils.ts) and sync-comment lists (the old mise.toml).
  versionErrors =
    let
      inherit (npm) waspcVersion;
      inherit (toolchain) nodeVersion;

      packageJsonVersionOf = dir: (lib.importJSON (dir + "/package.json")).version or "missing";
      checkPackageVersion =
        dir:
        lib.optional (packageJsonVersionOf dir != waspcVersion)
          "${toString dir}/package.json version ${packageJsonVersionOf dir} != waspc version ${waspcVersion}";

      nodeEnginesOf = file: (lib.importJSON file).engines.node or "missing";
      checkNodeEngines =
        file:
        lib.optional (nodeEnginesOf file != ">=${nodeVersion}")
          "${toString file} engines.node ${nodeEnginesOf file} != >=${nodeVersion} (the flake's Node version)";

      webNodeVersion = lib.trim (builtins.readFile ../web/.node-version);

      haskellNodeVersion =
        let
          m = builtins.match ".*oldestWaspSupportedNodeVersion = SV\\.Version ([0-9]+) ([0-9]+) ([0-9]+).*" (
            builtins.readFile ../waspc/src/Wasp/Node/Version.hs
          );
        in
        if m == null then "unparseable" else lib.concatStringsSep "." m;
    in
    lib.concatLists (
      map checkPackageVersion [
        ../waspc/data/packages/deploy
        ../waspc/data/packages/prisma
        ../waspc/data/packages/spec
        ../waspc/data/packages/studio
        ../waspc/data/packages/ts-inspect
        ../waspc/data/Generator/libs/auth
        ../waspc/data/Generator/libs/vite-ssr
      ]
    )
    ++ checkNodeEngines ../web/package.json
    ++ checkNodeEngines ../scripts/make-npm-packages/templates/main-package/package.json
    ++ checkNodeEngines ../scripts/make-npm-packages/templates/sub-package/package.json
    ++ lib.optional (webNodeVersion != nodeVersion)
      "web/.node-version (${webNodeVersion}) != the flake's Node version (${nodeVersion})"
    ++ lib.optional (haskellNodeVersion != nodeVersion)
      "Wasp.Node.Version's oldestWaspSupportedNodeVersion (${haskellNodeVersion}) != the flake's Node version (${nodeVersion})";
in
{
  inherit (project.native.checks) waspc-tests wasp-cli-tests;

  ormolu =
    pkgs.runCommand "check-ormolu" { nativeBuildInputs = [ toolchain.ormolu ]; }
      ''
        cd ${project.srcWithArtifacts}
        find . \( -name '*.hs' -o -name '*.hs-boot' \) -print0 \
          | xargs -0 ormolu --color always --check-idempotence --mode check
        touch $out
      '';

  cabal-gild =
    pkgs.runCommand "check-cabal-gild" { nativeBuildInputs = [ toolchain.cabal-gild ]; }
      ''
        cd ${../waspc}
        find . -name '*.cabal' -print0 | xargs -0 cabal-gild --mode check
        touch $out
      '';

  prettier =
    pkgs.runCommand "check-prettier" { nativeBuildInputs = [ toolchain.node ]; }
      ''
        cp -r ${../.} src
        chmod -R u+w src
        cd src
        ln -s ${npm.rootNodeModules}/node_modules node_modules
        ./node_modules/.bin/prettier --ignore-unknown --check --config prettier.config.mjs .
        touch $out
      '';

  versions-consistent =
    if versionErrors == [ ] then
      pkgs.runCommand "versions-consistent" { } "echo 'all version pins agree' > $out"
    else
      throw ''
        Version pins are inconsistent across the repo:
        ${lib.concatMapStrings (e: "  - ${e}\n") versionErrors}'';
}
// lib.optionalAttrs (project ? windows) {
  windows-unit-tests = pkgs.linkFarm "windows-unit-tests" (
    lib.mapAttrsToList (name: check: {
      inherit name;
      path = check;
    }) project.windows.checks
  );
}
