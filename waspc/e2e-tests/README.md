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

Tests execute their test cases in the `TestCaseDir`s.
The `TestCaseDir`s are created in the `waspc/e2e-tests/test-outputs/` directory.
While the exact files within a `TestCaseDir` aren't strictly defined, they usually have the following structure:

```
# Where:
#   `<name>` = the test's name.

e2e-tests/
└── test-outputs/
    └── <test-name>/<test-case-name>/  # test case directory
        ├── wasp-app/ # contains the Wasp app for that test
        └── ...
```

### Snapshot tests

We test Wasp application outputs primarily using snapshot tests.
Snapshot tests compare the current test outputs (`current` snapshot) against the expected test outputs (`golden` snapshot).

Snapshots are compared in two ways:

1. **By existence**: a file must exists in both the `current` and the `golden` snapshot.
2. **By content**: the file contents must be identical in both the `current` and the `golden` snapshot.

For more details, check out the `waspc/e2e-tests/E2E/Snapshot.hs` file.

Snapshot tests are executed in the `SnapshotDir`.
The `SnapshotDir`s are created in the `waspc/e2e-tests/test-outputs/snapshots/` directory.
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
    [ test "fail-outside-project" $ waspFails ["start"],
      test "succeed-compiled-project" $
        withNewWaspProject minimalStarterTemplate $ do
          wasp ["compile"]
          let ports = Ports.waspStartCompiledProject
          app <- startInBackground $ waspCli ("start" : appPortArgs ports)
          waitUntilListening (appPortList ports) 300
          waspPidFromLockFile >>= signal SIGINT
          waitForExit app 60
    ]

waspNewSnapshotTest :: E2ETest
waspNewSnapshotTest = snapshotTest "wasp-new" $ createWaspProject minimalStarterTemplate
```

- Each test runs in its own directory, `test-outputs/<group>/<test>/`, and logs every command it runs and its output to `output.log` there. Snapshot tests log to `test-outputs/snapshots/<name>.log`.
- Processes started with `startInBackground` are stopped when the test ends, however it ends.
- Tests that run an app get their own ports from `E2E/Ports.hs`, so they can run in parallel. Tests that can't run at the same time go in a `sequentialTestGroup`.
- Use `skipIfDockerDisabled` in tests that need Docker.

Some tests still use the older shell commands DSL (`ShellCommands.hs`) while we port them (see [#3404](https://github.com/wasp-lang/wasp/issues/3404)).

## Running tests

- `./run test:waspc:e2e` runs all of the tests.
- `./run test:waspc:e2e "<pattern>"` runs only the tests that match the [tasty pattern](https://github.com/UnkindPartition/tasty#patterns), e.g. `"/wasp-start/"`.
- `./run test:waspc:e2e:accept-all` regenerates all golden snapshots.
- `WASP_E2E_TEST_MAX_JOBS` limits how many tests run at once.
- `WASP_E2E_TESTS_SKIP_DOCKER=1` skips the tests that need Docker.
