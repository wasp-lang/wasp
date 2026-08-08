# The single source of truth for Wasp's dev tool versions.
#
# Tools come from three places:
#  - Node is repackaged from the official nodejs.org binaries so we control
#    the exact version (it must equal Wasp's lowest supported Node version,
#    which nixpkgs rarely carries at the exact patch level).
#  - GHC comes from nixpkgs' `haskell.compiler.ghc967` (prebuilt on
#    cache.nixos.org). The `nix build` packaging path uses haskell.nix's own
#    GHC of the same version instead (see project.nix); the shells only need
#    a compiler for cabal to drive.
#  - Everything else comes from nixpkgs, prebuilt on cache.nixos.org. Exact
#    tool versions follow the nixpkgs pin instead of being chosen by us; when
#    bumping the flake inputs, check that formatter output is unchanged
#    (ormolu especially) or land the reformat in the same commit. If some
#    nixpkgs version is ever unusable, that one tool can be built at an exact
#    version with `pkgs.haskell-nix.tool` instead (source build).
{ pkgs }:
let
  inherit (pkgs) lib stdenv;

  # This should be equal to Wasp's lowest supported Node version.
  # Keep in sync with:
  # - The compiler (Wasp.Node.Version).
  # - The docs: /docs/introduction/quick-start.md ("Requirements" section).
  # - The docs plugin: /web/src/remark/search-and-replace.ts
  # - The web CI: /web/.node-version
  # - The web package: /web/package.json
  # - The wasp-cli package templates: /scripts/make-npm-packages/templates/*/package.json
  # - The embedded packages: /waspc/data/packages/*/package.json
  # (Also just grep for the version in case we forgot somewhere.)
  nodeVersion = "24.14.1";

  nodePlatforms = {
    "x86_64-linux" = {
      name = "linux-x64";
      hash = "sha256-hNOHFdRJRHEX0Fw+cazXjapJ1bG/qKrPYQMDkgwzIr4=";
    };
    "aarch64-linux" = {
      name = "linux-arm64";
      hash = "sha256-ceQn4ot4hG8gHU1ezDDLE9FQjKCZ7zhxiJoSVsfW9n4=";
    };
    "x86_64-darwin" = {
      name = "darwin-x64";
      hash = "sha256-qHo3oQwvr2V0LH1YEvW6uHju5SsN/99Xj0m3qAjZbd0=";
    };
    "aarch64-darwin" = {
      name = "darwin-arm64";
      hash = "sha256-Di5nnXZ0PW2SJeYTJ6HdwyTkqJqAiRx4wzcghgHZj3c=";
    };
  };

  # Official prebuilt Node (includes the bundled npm/npx). Hashes come from
  # https://nodejs.org/dist/v<version>/SHASUMS256.txt (the .tar.xz entries).
  node =
    let
      platform =
        nodePlatforms.${stdenv.hostPlatform.system}
          or (throw "No Node binary distribution known for ${stdenv.hostPlatform.system}");
    in
    stdenv.mkDerivation {
      pname = "nodejs-bin";
      version = nodeVersion;
      src = pkgs.fetchurl {
        url = "https://nodejs.org/dist/v${nodeVersion}/node-v${nodeVersion}-${platform.name}.tar.xz";
        inherit (platform) hash;
      };
      nativeBuildInputs = lib.optionals stdenv.hostPlatform.isLinux [ pkgs.autoPatchelfHook ];
      buildInputs = lib.optionals stdenv.hostPlatform.isLinux [ stdenv.cc.cc.lib ];
      dontConfigure = true;
      dontBuild = true;
      installPhase = ''
        runHook preInstall
        mkdir -p $out
        cp -r bin lib include share $out/
        chmod -R u+w $out

        # npm/npx/corepack ship with `#!/usr/bin/env node` shebangs, which
        # don't work in Nix build sandboxes (no /usr/bin/env there). Point
        # them at our own node. (The stdenv's automatic shebang patching
        # can't do this: it resolves interpreters via buildInputs, and this
        # derivation's node can't be its own build input.)
        for script in \
          "$out/lib/node_modules/npm/bin/npm-cli.js" \
          "$out/lib/node_modules/npm/bin/npx-cli.js" \
          "$out/lib/node_modules/corepack/dist/corepack.js"; do
          sed -i "1s|^#!.*|#!$out/bin/node|" "$script"
        done

        runHook postInstall
      '';
      passthru = {
        version = nodeVersion;
        # importNpmLock.buildNodeModules expects nodejs.passthru.python
        # (used by node-gyp for native modules).
        python = pkgs.python3;
      };
      meta.mainProgram = "node";
    };

  # nixpkgs' name for GHC 9.6.7 (used by the shells) — haskell.nix uses the
  # same name for its own GHC (used by the `nix build` path, see project.nix).
  # When upgrading GHC, update both this file and project.nix stays in sync
  # automatically since both read this value.
  haskellCompilerName = "ghc967";

  # Slims a Haskell executable's runtime closure down to the binary itself.
  staticExe = pkgs.haskell.lib.justStaticExecutables;
in
{
  inherit node nodeVersion haskellCompilerName;

  ghc = pkgs.haskell.compiler.${haskellCompilerName};
  cabal = pkgs.cabal-install;

  inherit (pkgs) jq;

  # Formatters & linters.
  inherit (pkgs) ormolu hlint;
  # nixpkgs carries cabal-gild 1.6.x, which predates positional file
  # arguments (`cabal-gild --mode check FILE...`), so we pin the exact
  # version ourselves. It builds in a couple of minutes against nixpkgs'
  # default (cached) Haskell package set.
  cabal-gild = staticExe (
    pkgs.haskellPackages.callPackage (
      {
        mkDerivation,
        base,
        bytestring,
        Cabal-syntax,
        containers,
        exceptions,
        filepath,
        filepattern,
        parsec,
        pretty,
        text,
        transformers,
      }:
      mkDerivation {
        pname = "cabal-gild";
        version = "1.8.4.1";
        sha256 = "07axmqsdm2sgpwbz52y2cy281xxk0fhp3vnsw7fyp7ndn0786h8q";
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base
          bytestring
          Cabal-syntax
          containers
          exceptions
          filepath
          filepattern
          parsec
          pretty
          text
          transformers
        ];
        executableHaskellDepends = [ base ];
        doCheck = false;
        description = "Formats package descriptions";
        license = pkgs.lib.licenses.mit;
        mainProgram = "cabal-gild";
      }
    ) { }
  );
  stan = staticExe pkgs.haskellPackages.stan;

  # Dev utilities.
  inherit (pkgs) ghcid graphviz;
  graphmod = staticExe pkgs.haskellPackages.graphmod;

  # HLS built against the same GHC version as the project. NOTE: non-default
  # GHC versions are usually not prebuilt on cache.nixos.org, so the first
  # `nix develop` may compile HLS locally (one-time cost).
  haskell-language-server = pkgs.haskell-language-server.override {
    supportedGhcVersions = [ (lib.removePrefix "ghc" haskellCompilerName) ];
  };
}
