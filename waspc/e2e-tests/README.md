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

For more details, check out the `waspc/e2e-tests/E2E/Snapshot.hs` file.

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

Tests are written in plain Haskell with the `E2E` module, which runs commands as processes (no shell) and checks their results:

```haskell
waspStartTest :: E2ETest
waspStartTest =
  testGroup
    "wasp-start"
    [ test "fail-outside-project" $ waspFailsOutsideProject ["start"],
      test "succeed-compiled-project" $
        withNewWaspProject minimalStarterTemplate $ do
          wasp ["compile"]
          ports <- allocateAppPorts
          app <- startInBackground $ waspCli ("start" : appPortArgs ports)
          waitUntilListening app (appPortList ports) appStartTimeoutSeconds
          getWaspPidFromLockFile >>= signalPid SIGINT
          waitUntilStopped app (appPortList ports)
    ]

waspNewSnapshotTest :: E2ETest
waspNewSnapshotTest = snapshotTest "wasp-new" $ createWaspProject minimalStarterTemplate
```

- Each test runs in its own directory, `test-outputs/<group>/<test>/`, and logs every command it runs and its output to `output.log` there. Snapshot tests log to `test-outputs/snapshots/<name>.log`. The run fails at the start if two tests would share a directory, or if a test or group name isn't a single directory name.
- When a step fails, the test's failure message says which command failed and shows the last lines of its output.
- Long tests label their phases with `step "<what it checks>" $ do ...`. The label is logged as a header, and a failure in the phase starts with it.
- Processes started with `startInBackground` are stopped when the test ends, however it ends.
- Tests that run an app get free ports from `allocateAppPorts` (or `allocatePort`, for a command that takes a single port), so they can run in parallel, also with the tests of other checkouts. Tests that can't run at the same time go in a `sequentialTestGroup`.
- Tests that need Docker are made with `dockerTest` instead of `test`, and pass without running when Docker is disabled. Snapshot tests must produce a snapshot, so they use `whenDockerEnabled` for their Docker steps.
- Resources outside the test directory, like Docker images, get a name from `makeUniqueName`, so tests of other checkouts can't use the same one, and a `registerCleanupStep` that removes them, registered before the step that creates them. Cleanup steps run when the test ends and can't fail it: a failing one is reported in the log and on stderr, so make them tolerate resources that are already gone. Nothing can cut a hung cleanup short, so give their commands the short `cleanupCommandTimeoutSeconds` (`dockerCleanup` does).
- Timeouts and other tuning numbers are named constants (e.g. `failFastTimeoutSeconds`, `settleTimeoutSeconds`), defined once in `E2E/` when more than one module uses them for the same reason.
- `wasp` commands run without the runner's `DATABASE_URL`, so they can't act on a database outside the test. Tests that need one set it with `withEnv`.

### Where helpers go

A helper that isn't specific to one test goes in `E2E/`, in the module for what it's about:

- `E2E/TestM.hs`: the test monad and what every step can do: fail, label a phase (`step`), log, run in a directory, register cleanups, make unique names, and poll.
- `E2E/Command.hs`: describing commands, and the timeouts commands run with (e.g. `cleanupCommandTimeoutSeconds`). Timeouts of waits sit next to the wait they're for.
- `E2E/Process.hs`: running commands and everything about a process's life (output, exit, signals).
- `E2E/Ports.hs`, `E2E/Net.hs`: allocating ports, and waiting for a process to listen on them or to stop listening (`waitUntilStopped`).
- `E2E/Docker.hs`: Docker commands, and tests and steps that need Docker.
- `E2E/Wasp.hs`: wrappers around Wasp CLI commands and the tools a Wasp project runs (e.g. `viteBuildCommand`), and facts about Wasp projects (e.g. their migrations dirs).
- `E2E/Assertions.hs`, `E2E/Files.hs`: generic checks and file system steps.

A helper with a single caller stays in that test's module. Steps that write app content (models, TS files) shared by several tests go in `Tests/Fixtures.hs`.

## Running tests

- `mise run test:waspc:e2e` runs all of the tests.
- `mise run test:waspc:e2e "<pattern>"` runs only the tests that match the [tasty pattern](https://github.com/UnkindPartition/tasty#patterns), e.g. `"/wasp-start/"`.
- `mise run test:waspc:e2e:accept-all` runs all of the tests, and replaces the golden snapshots that differ from the current ones. Pass `--accept` to `test:waspc:e2e` to do that only for the tests that match a pattern.
  - It doesn't delete golden snapshots that no test uses anymore (e.g. of a removed test), or files that became ignored in the golden snapshots. Delete those by hand.
- Missing golden snapshots are created, also without `--accept`, except on CI: it sets `TASTY_NO_CREATE=true` (tasty-golden's `--no-create`), so there they fail the test.
- `WASP_CLI_CMD` sets the Wasp CLI under test. It must be a single executable (a path, or a program in `PATH`), since tests run it without a shell. By default, it's `tools/wasp-cli-dev`, which runs the current waspc source.
- `WASP_E2E_TEST_MAX_JOBS` (a positive integer) limits how many tests run at once.
- `WASP_E2E_TESTS_SKIP_DOCKER=1` skips the tests that need Docker (they pass, and the run says so when it starts). Unset, empty, or `0` runs them. Any other value fails the run.
