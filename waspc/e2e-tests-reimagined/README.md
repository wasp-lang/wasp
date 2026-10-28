# waspc e2e tests (reimagined)

End-to-end tests of the Wasp CLI, written in plain Haskell. Commands run as
processes (never through a shell), and file operations happen in Haskell.

## Running

From the `waspc` dir:

```sh
./run test:waspc:e2e-reimagined             # all tests
./run test:waspc:e2e-reimagined:plain       # only plain tests
./run test:waspc:e2e-reimagined:snapshots   # only snapshot tests
./run test:waspc:e2e-reimagined:accept      # only regenerate the golden snapshots
```

Each command takes optional patterns and then runs only tests whose name
contains one of them, e.g. `./run test:waspc:e2e-reimagined:accept kitchen-sink`.

The `./run` commands call the test binary with these options, which you can
also pass directly with `cabal test waspc-e2e-tests-reimagined --test-options='...'`:

- `--kind plain|snapshot`: only run tests of this kind.
- `--accept`: replace golden snapshots with the current ones.
- `-m TEXT`: only run tests whose name contains `TEXT`. Repeatable.
- `--jobs N`: at most `N` tests at once.
- `--list`: list the tests and their resources instead of running them.

Progress is printed as each test finishes:

```
[ 12/73  16%] PASS wasp-build/fail-sqlite-project (14.2s)
```

Environment variables:

- `WASP_CLI_CMD`: the Wasp CLI to test. Defaults to the `wasp-cli` Cabal builds for the suite.
- `WASP_E2E_TEST_MAX_JOBS`: default for `--jobs`. Defaults to half of the CPU cores.
- `WASP_E2E_TESTS_SKIP_DOCKER`: skips Docker steps and tests that require Docker.

Outputs go to `test-outputs/`: `tests/<group>/<test>/` for tests (with an
`output.log`), and `snapshots/<name>-current/` (with `<name>.log` next to it)
for snapshot tests.

## Writing tests

```haskell
waspCleanTests :: [E2eTest]
waspCleanTests =
  testGroup
    "wasp-clean"
    [ test "fail-outside-project" $ waspFails ["clean"],
      test "succeed-compiled-project" $
        withNewWaspProject minimalStarterTemplate $ do
          wasp ["compile"]
          wasp ["clean"]
          assertDirectoryDoesNotExist ".wasp"
    ]
```

A snapshot test is the same, but declared with `snapshotTest`. Whatever its
steps leave in its directory is compared to the golden snapshot
`<name>-golden`.

```haskell
waspCompileSnapshotTest :: E2eTest
waspCompileSnapshotTest =
  snapshotTest "wasp-compile" $
    withNewWaspProject minimalStarterTemplate $ wasp ["compile"]
```

Register new tests in `Tests.hs`. Everything for writing tests is exported
from the `E2e` module.

## Parallelism

Every test runs in parallel with the others by default. When tests would
interfere with each other (e.g. they bind the same port), they declare a shared
`Resource`. The runner never runs more tests using a resource than its
capacity allows:

```haskell
usingResources [devDatabasePort] $ test "..." $ do ...
```

Tests start in the order of `Tests.hs` whenever a job slot and their resources
are free, so slow tests should come first.

## Long-running commands

```haskell
devDb <- startInBackground $ waspCli ["db", "start"]
waitForOutput devDb "listening on IPv4 address" 180
...
interrupt devDb
waitForExit devDb
```

Background processes still running when the test ends are stopped.
