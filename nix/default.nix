# Entry point for all of Wasp's Nix outputs, wired together per system.
#
# Module map (each file documents its own contents):
#   toolchain.nix    — pinned dev tools (GHC, cabal, node, formatters, ...)
#   npm.nix          — offline derivations for every npm lockfile in the repo
#   project.nix      — the haskell.nix project (native + musl static + mingw)
#   data-dir.nix     — the Cabal data dir, as shipped and as installed
#   wasp-cli.nix     — runnable `nix build .#wasp-cli` package
#   release.nix      — release tarballs (same layout the CI artifacts had)
#   bundle-macos.nix — makes macOS executables self-contained (no /nix/store)
#   apps.nix         — `nix run .#<cmd>` replacements for the old ./run script
#   shells.nix       — devShells.{default,ci,ci-no-haskell}
#   checks.nix       — `nix flake check` (unit tests, formatting, versions)
{ inputs, system }:
let
  pkgs = import inputs.nixpkgs {
    inherit system;
    overlays = [ inputs.haskellNix.overlay ];
    inherit (inputs.haskellNix) config;
  };
  inherit (pkgs) lib;

  toolchain = import ./toolchain.nix { inherit pkgs; };
  npm = import ./npm.nix { inherit pkgs toolchain; };
  project = import ./project.nix {
    inherit pkgs toolchain npm;
    self = inputs.self or null;
  };
  dataDir = import ./data-dir.nix { inherit pkgs toolchain npm project; };
  waspCli = import ./wasp-cli.nix { inherit pkgs toolchain project dataDir; };
  release = import ./release.nix {
    inherit
      pkgs
      lib
      project
      dataDir
      ;
  };
  appsModule = import ./apps.nix { inherit pkgs toolchain npm; };
  shells = import ./shells.nix { inherit pkgs toolchain; };
  checks = import ./checks.nix {
    inherit
      pkgs
      toolchain
      npm
      project
      ;
  };
in
{
  packages = {
    default = waspCli.wasp-cli;
    inherit (waspCli) wasp-cli wasp-cli-unwrapped;
    wasp-data-release = dataDir.release;
    wasp-data-runtime = dataDir.runtime;
    wasp-data-manifest = dataDir.manifest;
    # All the `nix run` command scripts in one buildable package — building
    # it shellchecks every command and downloads their tools.
    dev-scripts = pkgs.linkFarm "wasp-dev-scripts" appsModule.scripts;
  }
  // lib.optionalAttrs (project ? static) {
    wasp-cli-static = project.static.exe;
  }
  // lib.optionalAttrs (project ? windows) {
    wasp-cli-windows = project.windows.exe;
  }
  // release.tarballs;

  inherit (appsModule) apps;
  inherit checks;
  devShells = shells;
}
