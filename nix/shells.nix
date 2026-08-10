# Dev shells. All of them are plain "tools on PATH" shells: Haskell
# dependencies are still built by cabal from Hackage (and cached in cabal's
# store), exactly like before the Nix migration. We deliberately do NOT use
# haskell.nix's shellFor here — it would provide the whole dependency closure
# from Nix, but without a project binary cache that means compiling ~200
# packages with Nix on first entry. Revisit if/when a project cache exists.
#
# Shell inventory (mirrors the old mise config + its CI overlays):
#   default       — everything a contributor needs, including HLS.
#   ci            — Haskell CI jobs: compilers + formatters, no HLS/hlint/stan.
#   ci-no-haskell — Node-only CI jobs.
{ pkgs, toolchain }:
let
  nodeTools = [
    toolchain.node
    toolchain.jq
  ];

  haskellTools = [
    toolchain.ghc
    toolchain.cabal
    pkgs.pkg-config
  ];

  # C libraries of waspc's Haskell dependencies (just zlib today), which
  # cabal discovers through pkg-config — the Nix equivalent of the old mise
  # bootstrap's apt/apk zlib dev packages. These must be buildInputs (host
  # dependencies) for the pkg-config hook to expose their .pc files.
  haskellCLibs = [ pkgs.zlib ];

  formatters = [
    toolchain.ormolu
    toolchain.cabal-gild
  ];
in
{
  default = pkgs.mkShell {
    name = "wasp-dev";
    packages =
      nodeTools
      ++ haskellTools
      ++ formatters
      ++ [
        toolchain.hlint
        toolchain.stan
        toolchain.ghcid
        toolchain.haskell-language-server
      ];
    buildInputs = haskellCLibs;
  };

  ci = pkgs.mkShell {
    name = "wasp-ci";
    packages = nodeTools ++ haskellTools ++ formatters;
    buildInputs = haskellCLibs;
  };

  ci-no-haskell = pkgs.mkShell {
    name = "wasp-ci-no-haskell";
    packages = nodeTools;
  };
}
