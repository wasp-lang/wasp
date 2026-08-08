# `nix build .#wasp-cli` — the runnable, store-installed CLI.
#
# The binary itself only needs its data dir; we point the existing
# `waspc_datadir` override (see Wasp.Data / Paths_waspc) at the assembled
# runtime data dir, so no Haskell changes are involved. Node is *suffixed*
# onto PATH: your own node/npm wins if you have one (user apps resolve their
# tooling from it), but the CLI still works on a machine with no Node at all.
{
  pkgs,
  toolchain,
  project,
  dataDir,
}:
rec {
  wasp-cli-unwrapped = project.native.exe;

  wasp-cli =
    pkgs.runCommand "wasp-cli"
      {
        nativeBuildInputs = [ pkgs.makeWrapper ];
        meta.mainProgram = "wasp";
      }
      ''
        mkdir -p $out/bin
        makeWrapper ${wasp-cli-unwrapped}/bin/wasp-cli $out/bin/wasp \
          --set-default waspc_datadir ${dataDir.runtime}/data \
          --suffix PATH : ${pkgs.lib.makeBinPath [ toolchain.node ]}
        ln -s $out/bin/wasp $out/bin/wasp-cli
      '';
}
