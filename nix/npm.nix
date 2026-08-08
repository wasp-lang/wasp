# Offline (sandbox-safe) derivations for the npm projects embedded in the
# compiler's data dir. Dependency tarballs are derived from the committed
# package-lock.json files via nixpkgs' importNpmLock, so lockfile bumps are
# picked up automatically — there are no npmDepsHash values to maintain.
#
# Two kinds of outputs:
#  - "dist" builds — what `waspc/tools/packages/build.ts` (npm install +
#    npm run build) and `waspc/tools/libs/build.ts` (npm install + npm pack)
#    produce in the working tree. These get injected into the Haskell
#    project's source so Cabal's data-files globs match (see project.nix).
#  - runtime node_modules — prod-only dependency trees for the packages the
#    CLI executes out of its data dir at runtime. Shipping these pre-populated
#    is what keeps the CLI from running `npm install` *into the read-only Nix
#    store* on first use (Wasp.NodePackageFFI only checks that node_modules
#    exists).
{ pkgs, toolchain }:
let
  inherit (pkgs) lib importNpmLock;

  packagesDir = ../waspc/data/packages;
  libsDir = ../waspc/data/Generator/libs;

  # The waspc version, parsed from waspc.cabal the same way
  # `waspc/tools/get-waspc-version.ts` does (first line matching /^version:/).
  waspcVersion =
    let
      m = builtins.match ".*\nversion:[[:space:]]*([^[:space:]\n]+).*" (
        builtins.readFile ../waspc/waspc.cabal
      );
    in
    if m == null then throw "Couldn't parse `version:` out of waspc.cabal" else builtins.head m;

  # Mirrors assertPackageVersionMatchesWaspc from waspc/tools/utils.ts: every
  # embedded package must carry the same version as waspc.cabal.
  assertVersionMatchesWaspc =
    dir: drv:
    let
      packageJson = lib.importJSON (dir + "/package.json");
    in
    if packageJson.version or null == waspcVersion then
      drv
    else
      throw ''
        Package "${packageJson.name or (toString dir)}" version (${packageJson.version or "none"}) does not match
        the waspc version (${waspcVersion}). Please update it to match.
      '';

  # A derivation with the package's offline node_modules set up by
  # importNpmLock's hook (which runs `npm install --ignore-scripts` against
  # store-path tarballs, then `npm rebuild`).
  mkNpmDerivation =
    {
      name,
      src,
      buildPhase,
      installPhase,
      npmRebuildFlags ? [ ],
    }:
    pkgs.stdenv.mkDerivation {
      inherit
        name
        src
        buildPhase
        installPhase
        npmRebuildFlags
        ;
      npmDeps = importNpmLock { npmRoot = src; };
      nativeBuildInputs = [
        toolchain.node
        importNpmLock.npmConfigHook
      ];
    };

  # `npm run build` outputs (the dist/ dir) of a data/packages/* package —
  # the offline equivalent of waspc/tools/packages/build.ts.
  mkPackageDist =
    name: extraArgs:
    assertVersionMatchesWaspc (packagesDir + "/${name}") (
      mkNpmDerivation (
        {
          name = "wasp-package-${name}-dist";
          src = packagesDir + "/${name}";
          buildPhase = ''
            runHook preBuild
            npm run build
            runHook postBuild
          '';
          installPhase = ''
            runHook preInstall
            cp -r dist $out
            runHook postInstall
          '';
        }
        // extraArgs
      )
    );

  # The studio server embeds its client: the upstream `build` script installs
  # and builds ./client from the network (`npm --prefix ./client install`), so
  # we build the client as its own offline derivation and splice its dist in
  # where the `build:client` step would have put it.
  studioClient = mkNpmDerivation {
    name = "wasp-studio-client-dist";
    src = packagesDir + "/studio/client";
    buildPhase = ''
      runHook preBuild
      npm run build
      runHook postBuild
    '';
    installPhase = ''
      runHook preInstall
      cp -r dist $out
      runHook postInstall
    '';
  };

  # `npm pack` tarball of a data/Generator/libs/* lib — the offline
  # equivalent of waspc/tools/libs/build.ts (`prepare` runs the build).
  mkLibTarball =
    name:
    assertVersionMatchesWaspc (libsDir + "/${name}") (mkNpmDerivation {
      name = "wasp-lib-${name}-tgz";
      src = libsDir + "/${name}";
      buildPhase = ''
        runHook preBuild
        npm pack
        runHook postBuild
      '';
      installPhase = ''
        runHook preInstall
        mkdir -p $out
        cp ./*.tgz $out/
        runHook postInstall
      '';
    });

  # Production-only node_modules for a package the CLI runs from its data dir.
  # Install scripts are skipped: the only package with a meaningful install
  # script is @prisma/engines (downloads native engines from the network),
  # and the embedded prisma package never needs them — it only ever runs
  # `prisma format`, which has been WASM-based since Prisma 4.
  mkRuntimeNodeModules =
    name:
    importNpmLock.buildNodeModules {
      npmRoot = packagesDir + "/${name}";
      nodejs = toolchain.node;
      derivationArgs = {
        pname = "wasp-package-${name}-node-modules";
        npmInstallFlags = [ "--omit=dev" ];
        npmRebuildFlags = [ "--ignore-scripts" ];
      };
    };
in
{
  inherit waspcVersion;

  # Built dist/ trees, keyed by their directory name under data/packages/.
  packageDists = {
    deploy = mkPackageDist "deploy" { };
    prisma = mkPackageDist "prisma" {
      # Skip @prisma/engines' postinstall (network download of native
      # engines); tsc doesn't need them.
      npmRebuildFlags = [ "--ignore-scripts" ];
    };
    spec = mkPackageDist "spec" { };
    studio = mkPackageDist "studio" {
      # Replicates the package's `build` script with the network-touching
      # `build:client` step replaced by the prebuilt client dist.
      buildPhase = ''
        runHook preBuild
        cp -r --no-preserve=mode ${studioClient} public
        npm run remove:dist
        ./node_modules/.bin/tsc
        npm run copy:public
        runHook postBuild
      '';
    };
    ts-inspect = mkPackageDist "ts-inspect" { };
  };

  # `npm pack` tarballs, keyed by their directory name under
  # data/Generator/libs/.
  libTarballs = {
    auth = mkLibTarball "auth";
    vite-ssr = mkLibTarball "vite-ssr";
  };

  # Prod-only node_modules for the packages the CLI executes at runtime
  # (the RunnablePackages of Wasp.NodePackageFFI). `spec` is absent on
  # purpose: it's installed into the user's project, not run from the data
  # dir.
  runtimeNodeModules = {
    deploy = mkRuntimeNodeModules "deploy";
    prisma = mkRuntimeNodeModules "prisma";
    studio = mkRuntimeNodeModules "studio";
    ts-inspect = mkRuntimeNodeModules "ts-inspect";
  };

  # node_modules for the repo root (prettier and its plugins), used by the
  # pure formatting check.
  rootNodeModules = importNpmLock.buildNodeModules {
    npmRoot = ../.;
    nodejs = toolchain.node;
    derivationArgs.pname = "wasp-root-node-modules";
  };
}
