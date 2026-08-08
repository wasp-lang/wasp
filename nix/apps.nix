# `nix run .#<command>` apps — the successor of the old `waspc/run` script.
# Command bodies are carried over from that script; names swap `:` for `-`
# (flake attribute names can't contain colons), so `./run test:waspc:unit`
# is now `nix run .#test-waspc-unit`.
#
# Apps run against your *working tree*, not the flake's store copy: each app
# locates the checkout at runtime (see repoLocator) and most cd into waspc/.
# `wasp-cli` and `bust-libs-cache` intentionally keep your current directory,
# since they operate on the Wasp app you're standing in.
{
  pkgs,
  toolchain,
  npm,
}:
let
  inherit (pkgs) lib;

  # Locates the Wasp checkout. Running from inside the repo Just Works; from
  # anywhere else (e.g. `bust-libs-cache` inside some Wasp app), point
  # WASP_REPO_ROOT at your clone.
  repoLocator = ''
    REPOSITORY_ROOT="''${WASP_REPO_ROOT:-$(git rev-parse --show-toplevel 2>/dev/null || true)}"
    if [ -z "$REPOSITORY_ROOT" ] || [ ! -f "$REPOSITORY_ROOT/waspc/waspc.cabal" ]; then
      echo "Error: couldn't locate the wasp repository." >&2
      echo "Run this command from inside the repo, or set WASP_REPO_ROOT to your clone." >&2
      exit 1
    fi
    PROJECT_ROOT="$REPOSITORY_ROOT/waspc"
    # The dev CLI wrapper (`cabal run wasp-cli` under the hood). Exported
    # because the e2e tests and wasp-app-runner read it from the environment.
    WASP_CLI_CMD="$PROJECT_ROOT/tools/wasp-cli-dev"
    export WASP_CLI_CMD
  '';

  colors = ''
    # shellcheck disable=SC2034 # not every script uses every color
    RESET="\033[0m"
    # shellcheck disable=SC2034
    GREEN="\033[32m"
    # shellcheck disable=SC2034
    RED="\033[31m"
  '';

  mkScript =
    name:
    {
      runtimeInputs ? [ ],
      text,
      cdToProject ? true,
    }:
    pkgs.writeShellApplication {
      inherit name runtimeInputs;
      text = repoLocator + lib.optionalString cdToProject ''cd "$PROJECT_ROOT"'' + "\n" + text;
    };

  # Tool groups for runtimeInputs. Note: writeShellApplication *prepends*
  # these to PATH, so ambient tools (docker, editors' node, ...) stay
  # available. git is needed both to locate the repo and by the githash
  # Template Haskell splice during cabal builds.
  haskellBuildTools = [
    toolchain.ghc
    toolchain.cabal
    toolchain.node
    pkgs.git
  ];
  nodeTools = [
    toolchain.node
    pkgs.git
  ];

  buildPackagesCmd = ''node "$PROJECT_ROOT/tools/packages/build.ts"'';
  buildLibsCmd = ''node "$PROJECT_ROOT/tools/libs/build.ts"'';

  unitTestsCmd = "cabal test waspc-tests --test-options='--hide-successes'";
  e2eTestsCmd = "cabal test waspc-e2e-tests --test-options='--hide-successes'";
  cliTestsCmd = "cabal test wasp-cli-tests --test-options='--hide-successes'";
  libsTestsCmd = ''node "$PROJECT_ROOT/tools/libs/test.ts"'';
  packagesTestsCmd = ''node "$PROJECT_ROOT/tools/packages/test.ts"'';
  startersTestsCmd = ''(cd "$PROJECT_ROOT/starters-e2e-tests" && npm i && npm run test:dev)'';

  # Copies .env.server.example to .env.server, but only if the example exists
  # and .env.server isn't already present. Then installs deps and runs the
  # e2e tests of each example app.
  examplesTestsFns = ''
    copy_env_server_example_if_needed() {
      [ ! -f .env.server.example ] || [ -f .env.server ] || cp .env.server.example .env.server
    }

    run_examples_e2e_tests() {
      local paths=(
        "examples/tutorials/TodoApp"
        "examples/tutorials/TodoAppTs"
        "examples/waspello"
        "examples/waspleau"
        "examples/websockets-realtime-voting"
        "examples/ask-the-documents"
        "examples/kitchen-sink"
      )

      for path in "''${paths[@]}"; do
        if ! (
          cd "$REPOSITORY_ROOT/$path" \
            && copy_env_server_example_if_needed \
            && "$WASP_CLI_CMD" install \
            && npm run test
        ); then
          echo -e "''${RED}E2E tests failed in $path''${RESET}"
          return 1
        fi
      done
    }
  '';

  # Formatter command fragments (shared by format/check/code-check).
  # shellcheck directives: the git ls-files expansions are intentionally
  # unquoted so the file list word-splits into arguments.
  ormoluCmd = mode: ''
    # shellcheck disable=SC2046
    ormolu --color always --check-idempotence --mode ${mode} $(git ls-files '*.hs' '*.hs-boot')
  '';
  cabalGildCmd = mode: ''
    # shellcheck disable=SC2046
    cabal-gild --mode ${mode} $(git ls-files '*.cabal')
  '';
  prettierCmd = script: ''(cd "$REPOSITORY_ROOT" && npm ci && npm run ${script})'';

  formatterTools = [
    toolchain.ormolu
    toolchain.cabal-gild
    toolchain.node
    pkgs.git
  ];
  scripts = lib.mapAttrs mkScript {
  build = {
    runtimeInputs = haskellBuildTools;
    text = ''
      ${buildPackagesCmd}
      ${buildLibsCmd}
      cabal build all
    '';
  };

  build-hs = {
    runtimeInputs = haskellBuildTools;
    text = ''
      cabal build all
    '';
  };

  build-packages = {
    runtimeInputs = nodeTools;
    text = buildPackagesCmd;
  };

  build-libs = {
    runtimeInputs = nodeTools;
    text = buildLibsCmd;
  };

  install = {
    runtimeInputs = haskellBuildTools;
    text = ''
      ${buildPackagesCmd}
      ${buildLibsCmd}
      cabal install --overwrite-policy=always
    '';
  };

  wasp-cli = {
    runtimeInputs = haskellBuildTools;
    cdToProject = false;
    text = ''
      exec "$WASP_CLI_CMD" "$@"
    '';
  };

  ghcid = {
    runtimeInputs = haskellBuildTools ++ [ toolchain.ghcid ];
    text = ''
      ghcid --command=cabal repl
    '';
  };

  ghcid-test = {
    runtimeInputs = haskellBuildTools ++ [ toolchain.ghcid ];
    # --color always is needed for Tasty to turn on the coloring.
    text = ''
      ghcid -T=':main --color always' --command=cabal repl tests/TastyDiscoverDriver.hs
    '';
  };

  test = {
    runtimeInputs = haskellBuildTools;
    text =
      colors
      + examplesTestsFns
      + ''
        if cabal test \
          && echo 'Running e2e tests' \
          && ${startersTestsCmd} \
          && run_examples_e2e_tests \
          && ${libsTestsCmd} \
          && ${packagesTestsCmd}; then
          echo 'ALL TESTS PASSED'
        else
          echo 'SOME TESTS FAILED'
          exit 1
        fi
      '';
  };

  test-waspc = {
    runtimeInputs = haskellBuildTools;
    text = ''
      ${unitTestsCmd}
      ${e2eTestsCmd}
    '';
  };

  test-waspc-unit = {
    runtimeInputs = haskellBuildTools;
    # An optional first argument is a tasty test pattern, see
    # https://github.com/UnkindPartition/tasty#patterns
    text = ''
      if [ $# -eq 0 ]; then
        ${unitTestsCmd}
      else
        ${unitTestsCmd} --test-options "-p \"$1\""
      fi
    '';
  };

  test-waspc-e2e = {
    runtimeInputs = haskellBuildTools;
    text = e2eTestsCmd;
  };

  test-waspc-e2e-accept-all = {
    runtimeInputs = haskellBuildTools;
    # Accepts diffs in the CLI snapshot tests by deleting the current golden
    # output and re-running the snapshot tests to produce a new golden output.
    text = ''
      rm -rf "$PROJECT_ROOT"/e2e-tests/test-outputs/snapshots/*-golden
      ${e2eTestsCmd}
    '';
  };

  test-cli = {
    runtimeInputs = haskellBuildTools;
    text = cliTestsCmd;
  };

  test-libs = {
    runtimeInputs = nodeTools;
    text = libsTestsCmd;
  };

  test-packages = {
    runtimeInputs = nodeTools;
    text = packagesTestsCmd;
  };

  test-kitchen-sink = {
    runtimeInputs = haskellBuildTools;
    text = ''
      cd "$REPOSITORY_ROOT/examples/kitchen-sink"
      "$WASP_CLI_CMD" install
      npm run test
    '';
  };

  test-examples = {
    runtimeInputs = haskellBuildTools;
    text =
      colors
      + examplesTestsFns
      + ''
        run_examples_e2e_tests
      '';
  };

  test-starters = {
    runtimeInputs = haskellBuildTools;
    text = startersTestsCmd;
  };

  bust-libs-cache = {
    runtimeInputs = haskellBuildTools ++ [ toolchain.jq ];
    cdToProject = false;
    # Busts the npm cache for all @wasp.sh/lib-* packages in the current Wasp
    # app. `wasp install` must run before compile so @wasp.sh/spec matches the
    # CLI version. Old generated workspaces point at old lib tarballs, so
    # remove them from disk and lockfile first; compile then regenerates
    # .wasp/out with fresh tarballs.
    text = ''
      rm -rf .wasp/out node_modules/@wasp.sh/generated-server node_modules/wasp

      # We remove .wasp/out before `wasp install`, so package-lock entries for
      # generated workspaces and their local tarball deps become stale. Drop
      # them so npm rebuilds those entries from the freshly generated
      # package.json files.
      PACKAGE_LOCK_TMP=$(mktemp)
      if ! jq '
        def is_stale_generated_wasp_package:
          startswith("node_modules/@wasp.sh/lib-")
          or . == "node_modules/@wasp.sh/generated-server"
          or . == "node_modules/wasp"
          or . == ".wasp/out"
          or startswith(".wasp/out/");

        def without_wasp_lib_deps:
          if .dependencies then
            .dependencies |= with_entries(
              select(.key | startswith("@wasp.sh/lib-") | not)
            )
          else
            .
          end;

        .packages |= with_entries(
          select(.key | is_stale_generated_wasp_package | not)
          | .value |= without_wasp_lib_deps
        )
      ' package-lock.json > "$PACKAGE_LOCK_TMP"; then
        rm -f "$PACKAGE_LOCK_TMP"
        exit 1
      fi
      mv "$PACKAGE_LOCK_TMP" package-lock.json

      "$WASP_CLI_CMD" install

      # Compiling the project copies the new lib tarball to the out dir.
      "$WASP_CLI_CMD" compile

      # The --force flag tells `npm` to ignore local cache when installing.
      npm install --force
    '';
  };

  stan = {
    runtimeInputs = haskellBuildTools ++ [ toolchain.stan ];
    text = ''
      cabal build all --enable-tests --enable-benchmarks
      stan report "$@"
    '';
  };

  hlint = {
    runtimeInputs = [ toolchain.hlint ];
    text = ''
      hlint . "$@"
    '';
  };

  format = {
    runtimeInputs = formatterTools;
    text = ''
      ${ormoluCmd "inplace"}
      ${cabalGildCmd "format"}
      ${prettierCmd "format:prettier"}
    '';
  };

  check = {
    runtimeInputs = formatterTools;
    text = ''
      ${ormoluCmd "check"}
      ${cabalGildCmd "check"}
      ${prettierCmd "check:prettier"}
    '';
  };

  format-ormolu = {
    runtimeInputs = formatterTools;
    text = ormoluCmd "inplace";
  };

  check-ormolu = {
    runtimeInputs = formatterTools;
    text = ormoluCmd "check";
  };

  format-cabal = {
    runtimeInputs = formatterTools;
    text = cabalGildCmd "format";
  };

  check-cabal = {
    runtimeInputs = formatterTools;
    text = cabalGildCmd "check";
  };

  format-prettier = {
    runtimeInputs = nodeTools;
    text = prettierCmd "format:prettier";
  };

  check-prettier = {
    runtimeInputs = nodeTools;
    text = prettierCmd "check:prettier";
  };

  code-check = {
    runtimeInputs =
      haskellBuildTools
      ++ formatterTools
      ++ [
        toolchain.hlint
        toolchain.stan
      ];
    text =
      colors
      + ''
        check_ormolu() {
          ${ormoluCmd "check"}
        }
        check_cabal_gild() {
          ${cabalGildCmd "check"}
        }
        check_prettier() {
          ${prettierCmd "check:prettier"}
        }
        check_hlint() {
          hlint .
        }
        check_stan() {
          cabal build all --enable-tests --enable-benchmarks && stan report
        }

        # Run every check even when earlier ones fail, capturing exit codes
        # for the summary.
        set +e
        check_prettier
        PRETTIER_RESULT=$?
        check_ormolu
        ORMOLU_RESULT=$?
        check_cabal_gild
        CABAL_GILD_RESULT=$?
        check_hlint
        HLINT_RESULT=$?
        check_stan
        STAN_RESULT=$?
        set -e

        TOTAL_RESULT=$((PRETTIER_RESULT || ORMOLU_RESULT || CABAL_GILD_RESULT || HLINT_RESULT || STAN_RESULT))

        exit_status_to_string() {
          if (("$1" == 0)); then echo -e "''${GREEN}OK''${RESET}"; else echo -e "''${RED}FAIL''${RESET}"; fi
        }

        echo
        echo
        echo "======================================"
        echo "               SUMMARY"
        echo "======================================"
        echo
        echo -e "Formatter (prettier): $(exit_status_to_string "$PRETTIER_RESULT")"
        echo -e "Formatter (ormolu): $(exit_status_to_string "$ORMOLU_RESULT")"
        echo -e "Formatter (cabal-gild): $(exit_status_to_string "$CABAL_GILD_RESULT")"
        echo -e "Linter (hlint): $(exit_status_to_string "$HLINT_RESULT")"
        echo -e "Static analysis (stan): $(exit_status_to_string "$STAN_RESULT")"
        echo "-----------------------"
        echo -e "All together: $(exit_status_to_string "$TOTAL_RESULT")"

        exit "$TOTAL_RESULT"
      '';
  };

  module-graph = {
    runtimeInputs = [
      toolchain.graphmod
      toolchain.graphviz
      pkgs.git
    ];
    text = ''
      shopt -s globstar
      graphmod --quiet --prune-edges "$PROJECT_ROOT"/src/**/*.hs | dot -Gsize=60,60! -Tpng -o module-graph.png
      echo "Printed module graph to module-graph.png."
    '';
  };

  get-waspc-version = {
    runtimeInputs = nodeTools;
    text = ''
      node "$PROJECT_ROOT/tools/get-waspc-version.ts"
    '';
  };

  version-bump = {
    runtimeInputs = haskellBuildTools ++ [ toolchain.jq ];
    text = ''
      node "$PROJECT_ROOT/tools/version-bump.ts" "$@"
    '';
  };
  };
in
{
  inherit scripts;
  apps = lib.mapAttrs (name: script: {
    type = "app";
    program = "${script}/bin/${name}";
  }) scripts;
}
