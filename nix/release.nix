# Release artifacts: the per-platform tarballs that CI uploads and
# scripts/make-npm-packages turns into the @wasp.sh/wasp-cli npm packages.
#
# Layout is byte-compatible with what waspc/tools/make_binary_package.sh
# produced (`./wasp-bin` + `./data/...`), so everything downstream —
# make-npm-packages, .github/actions/install-wasp-cli-artifact, the GitHub
# release assets — keeps working unchanged. The data set deliberately ships
# *without* node_modules, exactly like before: released CLIs run from a
# writable install dir and npm-install their package deps on first use.
#
# Both Linux x86_64 artifact names carry the same fully-static musl binary:
# a static binary runs on every glibc and musl distro alike, which also
# retires the old "build in an ancient Ubuntu container for a low glibc
# floor" trick. macOS binaries are dynamic but post-processed to be
# self-contained (see bundle-macos.nix).
{
  pkgs,
  lib,
  project,
  dataDir,
}:
let
  system = pkgs.stdenv.hostPlatform.system;

  # A tarball whose root has the contents of binDir (wasp-bin [+ libs/])
  # next to data/.
  mkTarball =
    name: binDir:
    pkgs.runCommand "wasp-${name}.tar.gz" { } ''
      mkdir pkg
      cp -r --no-preserve=mode ${binDir}/. pkg/
      chmod +x pkg/wasp-bin
      mkdir -p pkg/data
      cp -r --no-preserve=mode ${dataDir.release}/data/. pkg/data/
      tar -czf $out -C pkg .
    '';

  exeAsBinDir =
    exe: exeName:
    pkgs.runCommand "wasp-bin-dir" { } ''
      mkdir $out
      cp ${exe}/bin/${exeName} $out/wasp-bin
    '';

  bundleMacos = import ./bundle-macos.nix { inherit pkgs; };

  tarballs =
    lib.optionalAttrs (system == "x86_64-linux") (
      let
        staticTarball = mkTarball "linux-x86_64" (exeAsBinDir project.static.exe "wasp-cli");
      in
      {
        release-tarball-linux-x86_64 = staticTarball;
        release-tarball-linux-x86_64-static = staticTarball;
        # CI smoke artifact only — Windows binaries are not released (yet).
        release-zip-windows-x86_64 =
          pkgs.runCommand "wasp-windows-x86_64.zip" { nativeBuildInputs = [ pkgs.zip ]; }
            ''
              mkdir pkg
              cp ${project.windows.exe}/bin/wasp-cli.exe pkg/wasp-bin.exe
              mkdir -p pkg/data
              cp -r --no-preserve=mode ${dataDir.release}/data/. pkg/data/
              (cd pkg && zip -r $out .)
            '';
      }
    )
    // lib.optionalAttrs (system == "aarch64-linux") {
      release-tarball-linux-aarch64 = mkTarball "linux-aarch64" (
        exeAsBinDir project.static.exe "wasp-cli"
      );
    }
    // lib.optionalAttrs (system == "x86_64-darwin") {
      release-tarball-macos-x86_64 = mkTarball "macos-x86_64" (bundleMacos project.native.exe);
    }
    // lib.optionalAttrs (system == "aarch64-darwin") {
      release-tarball-macos-aarch64 = mkTarball "macos-aarch64" (bundleMacos project.native.exe);
    };
in
{
  inherit tarballs;
}
