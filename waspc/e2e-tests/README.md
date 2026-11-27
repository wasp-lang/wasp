# End-to-End tests for `waspc`

The purpose of e2e tests is to **verify that the Wasp binary works as expected**.
We are not concerned with the internal implementation, only its interface and outputs.

## More on purpose

**Interface** is exposed through the Wasp CLI.
We want to test the behavior of all of the Wasp CLI commands.
Every command is treated as a black box.

The main **outputs** of `waspc` are Wasp applications.
We want to validate that CLI commands correctly generate or modify Wasp applications.
In addition to applications, we also cover secondary outputs, such as the installation and uninstallation of the CLI itself, `bash` completions, and more.

## Test variants

`waspc e2e-tests` consist of two different tests variants:

1. **Tests**: tests whose output we don't need to save
2. **Snapshot Tests**: tests whose output we want to save

### Tests

We test Wasp CLI commands whose outputs we can discard.

Each test runs in its own test directory in `waspc/e2e-tests/test-outputs/`.
While the exact files within a test directory aren't strictly defined, they usually have the following structure:

```
# Where:
#   `<group>` = the name of the test's group.
#   `<test>`  = the test's name.

e2e-tests/
└── test-outputs/
    └── <group>/<test>/  # test directory
        ├── wasp-app/ # contains the Wasp app for that test
        ├── output.log # the commands the test ran, and their output
        └── ...
```

### Snapshot tests

We test Wasp application outputs primarily using snapshot tests.
Snapshot tests compare the current test outputs (`current` snapshot) against the expected test outputs (`golden` snapshot).

Snapshots are compared in two ways:

1. **By existence**: a file must exist in both the `current` and the `golden` snapshot.
2. **By content**: the file contents must be identical in both the `current` and the `golden` snapshot.

For more details, check out the `waspc/e2e-tests/src/E2E/Snapshot.hs` file.

Snapshot tests run in their snapshot directory in `waspc/e2e-tests/test-outputs/snapshots/`.
While the exact files within a snapshot aren't strictly defined, they usually have the following structure:

```
# Where:
#   `<name>`          = the snapshot test's name.
#   `<snapshot-type>` = `current` or `golden`.

e2e-tests/
└── test-outputs/
    └── snapshots/
        └── <name>-<snapshot-type>/  # snapshot directory, e.g. `wasp-build-current`, `wasp-build-golden`
            ├── wasp-app/ # contains the Wasp app for that snapshot test
            ├── ...
            └── snapshot-file-list.manifest # lists the files that should exist in the snapshot directory
```

## Writing tests

Tests are written in Haskell using the `E2E` module (`src/E2E.hs`):

```haskell
test_waspStart :: TestTree
test_waspStart =
  testGroup
    "wasp-start"
    [ test "fail-outside-project" $ waspFailsOutsideProject ["start"],
      test "succeed-compiled-project" $
        withNewWaspProject minimalStarterTemplate $ do
          wasp ["compile"]
          ports <- allocateAppPorts
          app <- startInBackground $ waspCli ["start", "--client-port", show ports.clientPort, "--server-port", show ports.serverPort]
          waitUntilListening app (appPortList ports) appStartTimeoutSeconds
          getWaspPidFromLockFile >>= signalPid SIGINT
          waitUntilStopped app (appPortList ports)
    ]

test_waspNewSnapshot :: TestTree
test_waspNewSnapshot = snapshotTest "wasp-new" $ createWaspProject minimalStarterTemplate
```

- To add a test, export `test_<name> :: TestTree` from a `src/Tests/*Test.hs` module (or `src/Tests/SnapshotTests/*Test.hs` for snapshot tests) and list the module in `waspc/waspc.cabal`.
- Tests run in parallel by default. Allocate free ports with `allocateAppPorts` / `allocatePort`, or wrap tests that can't overlap in `sequentialTestGroup`.
- Processes started with `startInBackground` are stopped automatically when a test ends. For external resources (like Docker containers or images), use `makeUniqueName` and register teardown before creation with `registerCleanupStep` (use `dockerCleanup` for Docker).
- Use `dockerTest` instead of `test` for tests that require Docker, or `whenDockerEnabled` inside snapshot tests.
- Use `E2E`'s assertions (`shouldBe`, `assertOutputContains`, `assertFileExists`, etc.) for checks, and `step` to label phases in longer tests.
- Put reusable helpers in `src/E2E/` (and re-export them from `src/E2E.hs`), shared test app fixtures in `src/Tests/Fixtures.hs`, and test-specific helpers in the test module.

## Running tests

- `mise run test:waspc:e2e` runs all tests.
- `mise run test:waspc:e2e "<pattern>"` runs tests matching a [pattern](https://github.com/UnkindPartition/tasty#patterns), e.g. `"/wasp-start/"`.
- `mise run test:waspc:e2e:accept-all` creates or updates all golden snapshots (pass `--accept` to `test:waspc:e2e` to update a subset). Removed snapshots and newly ignored files must be deleted manually.

Environment variables:

- `WASP_CLI_CMD`: overrides the Wasp CLI executable under test (must be a single executable; defaults to `tools/wasp-cli-dev`).
- `WASP_E2E_TEST_MAX_JOBS`: limits how many tests run in parallel.
- `WASP_E2E_TESTS_SKIP_DOCKER=1`: skips tests that require Docker.
