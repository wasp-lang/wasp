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

`waspc e2e-tests` consist of three different tests variants:

1. **Tests**: tests whose output we don't need to save
2. **Snapshot Tests**: tests whose output we want to save
3. **Transcript Tests**: tests that save what the commands print

### Tests

We test Wasp CLI commands whose outputs we can discard.

Tests execute their test cases in the `TestCaseDir`s.
The `TestCaseDir`s are created in the `waspc/e2e-tests/test-outputs/` directory.
A `TestCaseDir` is removed when its test case passes and kept when it fails, so you can inspect what went wrong.
To keep the `TestCaseDir`s of passed test cases too, set `WASP_E2E_TESTS_KEEP_OUTPUTS=1`.
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

For more details, check out the `waspc/e2e-tests/SnapshotTest.hs` file.

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

### Transcript tests

We test what the Wasp CLI commands print using transcript tests.
A transcript holds everything a command printed and its exit code.
Transcript tests compare the `current` transcripts against the `golden` transcripts, the same way snapshot tests do.

Each command is recorded twice:

1. **In a terminal** (through `script`): the way a user sees it. Skipped where `script` isn't available.
2. **Through a pipe**: stdout and stderr are recorded separately.

Only what Wasp itself prints is compared exactly.
The output of the tools Wasp runs (e.g. `npm install`) changes between runs, so each run of lines with the same job prefix is replaced by a placeholder line.
The output of the running app is left out.

For more details, check out the `waspc/e2e-tests/TranscriptTest.hs` file.

Transcripts live in the `waspc/e2e-tests/test-outputs/snapshots/` directory, and the commands run in the `waspc/e2e-tests/test-outputs/` directory:

```
# Where:
#   `<name>`          = the transcript test's name.
#   `<mode>`          = `terminal` or `pipe`.
#   `<snapshot-type>` = `current` or `golden`.

e2e-tests/
└── test-outputs/
    ├── transcript-<name>/<mode>/  # the directory the test's commands run in
    └── snapshots/
        └── transcripts-<snapshot-type>/
            └── <name>.<mode>.txt  # transcript file, e.g. `compile.terminal.txt`
```
