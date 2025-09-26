# End-to-End tests for `waspc`

The purpose of these `e2e-tests` is to verify the functionality of the **`waspc` binary**.
We are not concerned with the internal implementation of the binary, only its interface and outputs.

## More on purpose

**Interface** is exposed through the Wasp CLI. We want to test the behavior of all of the Wasp CLI commands. These tests test behavior at the command level, rather than the internal implementation details.

The main **outputs** of the `waspc` binary are Wasp applications. We want to validate that CLI commands correctly generate or modify applications in line with expectations.
In addition to applications, we also cover secondary outputs, such as the installation and uninstallation of the CLI itself, `bash` completions, and more.

## Snapshot tests

We primarily test the `waspc` binary outputs using snapshot tests.
Snapshot tests compare the current test outputs (`current` snapshot) against the expected test outputs (`golden` snapshot).

Snapshots are compared in two ways:

1. **By existence**: a file must exists in both the `current` and the `golden` snapshot.
2. **By content**: the file contents must be identical in both the `current` and the `golden` snapshot.

For more details, check out the `waspc/e2e-tests/SnapshotTest.hs` file.

Snapshots are saved in the `waspc/e2e-tests/snapshots/` directory.
While the exact files within a snapshot aren’t strictly defined, they usually have the following structure:

```yaml
# Where:
#   `<name>`          = the snapshot test's name.
#   `<snapshot-type>` = `current` or `golden`.

e2e-tests/
└── snapshots/
    └── <name>-<snapshot-type>/  # snapshot dirctory, e.g. `wasp-build-current`, `wasp-build-golden`
        ├── wasp-app/
        └── snapshot-file-list.manifest
```

`wasp-app` contains the Wasp app for that snapshot.
`snapshot-file-list.manifest` lists the files that should exist in the snapshot directory.
