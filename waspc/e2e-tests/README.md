# End-to-End Tests for `waspc`

The purpose of `e2e-tests` is to verify the functionality of the **`waspc` binary**.
We are not concerned with the internal implementation of the binary, only its interface and outputs.

## Functionalities

The e2e tests serve several critical purposes:

1. **E2E Testing**: verify that the `waspc` binary works as expected e2e.
2. **Regression Testing**: catch changes in `waspc` binary (code generation, CLI behavior).
3. **CLI Testing**: validate that the `waspc` binary CLI commands work correctly.

## Snapshot Tests

Snapshot tests compare the current test outputs (`current` snapshot) against the expected test outputs (`golden` snapshot).

Snapshots are compared in two ways:

1. **By existence**: a file must exists in both the `current` and the `golden` snapshot.
2. **By content**: the file contents must be identical in both the `current` and the `golden` snapshot.

For more details, check out the `waspc/e2e-tests/SnapshotTest.hs` file.

Snapshots are saved in the `waspc/e2e-tests/snapshots/` directory, which follows the next structure:

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
