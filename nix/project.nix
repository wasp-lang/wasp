# The haskell.nix project: how `nix build` compiles the compiler itself.
#
# The same project definition serves four targets:
#  - the native build (packages.wasp-cli, checks),
#  - fully-static musl builds for the Linux release binaries
#    (projectCross.musl64 / aarch64-multiplatform-musl),
#  - a Windows cross-build (projectCross.mingwW64; Template Haskell splices
#    run under wine automatically via haskell.nix's remote iserv).
#
# haskell.nix drives the real cabal solver against the index-state pinned in
# waspc/cabal.project, so dependency resolution here is identical to what
# plain cabal does in the dev shell.
{
  pkgs,
  toolchain,
  npm,
  self,
}:
let
  inherit (pkgs) lib;

  # The waspc source as Cabal needs to see it: the git-tracked tree plus the
  # npm build artifacts injected exactly where a working-tree build would
  # produce them. This is required, not cosmetic — waspc.cabal's data-files
  # globs (packages/*/dist/**, Generator/libs/**/*.tgz) must match real files
  # for any cabal packaging step to succeed.
  srcWithArtifacts =
    pkgs.runCommand "waspc-src-with-artifacts" { }
      (
        ''
          cp -r ${../waspc} $out
          chmod -R u+w $out
        ''
        + lib.concatStrings (
          lib.mapAttrsToList (name: dist: ''
            cp -r --no-preserve=mode ${dist} $out/data/packages/${name}/dist
          '') npm.packageDists
        )
        + lib.concatStrings (
          lib.mapAttrsToList (name: tgz: ''
            cp --no-preserve=mode ${tgz}/*.tgz $out/data/Generator/libs/${name}/
          '') npm.libTarballs
        )
      );

  # The commit the flake is built from, when the tree is clean (release/CI
  # builds always are). Embedded via GitRev.hs's build-env fallback since
  # githash can't see a .git dir inside the build sandbox.
  gitRev = if self != null then self.rev or null else null;

  project = pkgs.haskell-nix.cabalProject' {
    name = "waspc";
    src = srcWithArtifacts;
    compiler-nix-name = toolchain.haskellCompilerName;
    modules = [
      (
        { lib, ... }:
        {
          # The e2e/golden tests need network access and a writable project
          # dir, so they can't run inside `nix build`; they stay an impure
          # command (`nix run .#test-waspc-e2e`).
          packages.waspc.components.tests.waspc-e2e-tests.doCheck = false;
        }
        // lib.optionalAttrs (gitRev != null) {
          packages.waspc.components.library.preBuild = ''
            export WASP_BUILD_GIT_REV=${gitRev}
          '';
        }
      )
      # Fully-static executable for the musl cross-builds (the Linux release
      # binaries). Mirrors the old Alpine/BUILD_STATIC=1 setup, with the
      # static gmp/zlib/libffi coming from nixpkgs instead of apk packages.
      (
        { pkgs, lib, ... }:
        lib.mkIf pkgs.stdenv.hostPlatform.isMusl {
          packages.waspc.components.exes.wasp-cli = {
            enableShared = false;
            configureFlags = [
              "--disable-executable-dynamic"
              "--ghc-option=-optl=-static"
              "--ghc-option=-optl=-pthread"
              "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
              "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
              "--ghc-option=-optl=-L${pkgs.libffi.overrideAttrs (_: { dontDisableStatic = true; })}/lib"
            ];
          };
        }
      )
    ];
  };

  # The musl cross-project that produces this system's static Linux binary.
  staticCrossName =
    {
      "x86_64-linux" = "musl64";
      "aarch64-linux" = "aarch64-multiplatform-musl";
    }
    .${pkgs.stdenv.hostPlatform.system} or null;

  mkOutputs = p: {
    inherit (p) hsPkgs;
    exe = p.hsPkgs.waspc.components.exes.wasp-cli;
    checks = {
      waspc-tests = p.hsPkgs.waspc.checks.waspc-tests;
      wasp-cli-tests = p.hsPkgs.waspc.checks.wasp-cli-tests;
    };
  };
in
{
  inherit project srcWithArtifacts;
  native = mkOutputs project;
}
// lib.optionalAttrs (staticCrossName != null) {
  static = mkOutputs project.projectCross.${staticCrossName};
}
// lib.optionalAttrs (pkgs.stdenv.hostPlatform.system == "x86_64-linux") {
  windows = mkOutputs project.projectCross.mingwW64;
}
