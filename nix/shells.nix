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
  ];

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
  };

  ci = pkgs.mkShell {
    name = "wasp-ci";
    packages = nodeTools ++ haskellTools ++ formatters;
  };

  ci-no-haskell = pkgs.mkShell {
    name = "wasp-ci-no-haskell";
    packages = nodeTools;
  };
}
