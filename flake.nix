{
  description = "Wasp — dev toolchain, build system, and packaging";

  nixConfig = {
    # The IOG cache serves prebuilt GHC (and other haskell.nix infrastructure)
    # for the nixpkgs pin we follow below. Without it, GHC builds from source.
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    # haskell.nix computes the cabal build plan in a derivation and imports it
    # back into the evaluation (IFD).
    allow-import-from-derivation = true;
  };

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    # Following haskell.nix's own nixpkgs pin is load-bearing: cache.iog.io
    # only has GHC builds for exactly that pin.
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    inputs@{ flake-utils, ... }:
    flake-utils.lib.eachSystem
      [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ]
      (system: import ./nix { inherit inputs system; });
}
