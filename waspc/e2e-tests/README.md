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

## Test varaints

`waspc e2e-tests` consist of two different tests variants:

1. **Ephemeral Tests**: tests whose output we don't need to save
2. **Snapshot Tests**: tests whose output we want to save

### Ephemeral tests

You can think of ephemeral tests as "normal" e2e tests.
We test Wasp CLI commands whose outputs we can discard.

Ephemeral tests are executed in the `EhpemeralDir`.
Ther `EhpemeralDir`s are created in the `waspc/e2e-tests/EphemeralTest/` diectory.
While the exact files within a `EhpemeralDir` aren’t strictly defined, they usually have the following structure:

```yaml
# Where:
#   `<name>` = the ephemeral test's name.

e2e-tests/
└── EphemeralTest/
    └── temp-<name>/  # ephemeral dirctory, e.g. `temp-wasp-info`
        ├── wasp-app/ # contains the Wasp app for that ephemeral test.
        └── ...
```

`wasp-app` contains the Wasp app for that ephemeral test.

`EhpemeralDir`s are supposed to be cleaned up after successful exeuction or before subsequent `waspc e2e-tests` runs.
The exception is when an ephemeral tests fails, we keep the directory to make debugging easier (in case the tests generates outputs).


### Snapshot tests

We test Wasp application outputs primarily using snapshot tests.
Snapshot tests compare the current test outputs (`current` snapshot) against the expected test outputs (`golden` snapshot).

Snapshots are compared in two ways:

1. **By existence**: a file must exists in both the `current` and the `golden` snapshot.
2. **By content**: the file contents must be identical in both the `current` and the `golden` snapshot.

For more details, check out the `waspc/e2e-tests/SnapshotTest.hs` file.

Snapshot tests are executed in the `SnapshotDir`.
The `SnapshotDir`s are created in the `waspc/e2e-tests/SnapshotTest/snapshots/` directory.
While the exact files within a snapshot aren’t strictly defined, they usually have the following structure:

```yaml
# Where:
#   `<name>`          = the snapshot test's name.
#   `<snapshot-type>` = `current` or `golden`.

e2e-tests/
└── SnapshotTest/
    └── snapshots/
        └── <name>-<snapshot-type>/  # snapshot dirctory, e.g. `wasp-build-current`, `wasp-build-golden`
            ├── wasp-app/ # contains the Wasp app for that snapshot test
            ├── ...
            └── snapshot-file-list.manifest # lists the files that should exist in the snapshot directory
```