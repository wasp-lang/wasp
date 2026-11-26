# Wasp.PackageManager

Import `Wasp.PackageManager`. The other modules are internal.

## Detection

Detects which package manager a project uses.

```haskell
detectProjectPackageManager  :: Path' Abs (Dir d) -> IO (PackageManager, Maybe DetectionWarning)
detectInvokingPackageManager :: IO (PackageManager, Maybe DetectionWarning)
```

- `detectProjectPackageManager` is for commands that run in an existing project.
- `detectInvokingPackageManager` is for scaffolding, when there are no project files yet. It only reads `npm_config_user_agent`.

Both give the package manager to use, so the caller has nothing left to decide. The warning is there when the detected package manager isn't supported, and the caller should show it.

### Flow

```mermaid
flowchart LR
  start(["dir"]) --> checkStart["Check dir"]
  checkStart -->|nothing| workspaceRoots["Check workspace roots above dir"]
  workspaceRoots -->|nothing| userAgent["Read npm_config_user_agent"]
  userAgent -->|nothing| npm(["npm"])
  checkStart -->|found| isSupported{"Supported?"}
  workspaceRoots -->|found| isSupported
  userAgent -->|found| isSupported
  isSupported -->|yes| detected(["The detected one"])
  isSupported -->|no| npmWithWarning(["npm, with a warning"])
```

npm, pnpm, Yarn and Bun are all detected, but only the ones in `supportedPackageManagers` are used. For now that is npm.

Checking the workspace roots above `dir`:

```mermaid
flowchart TD
  start(["dir"]) --> isPastBoundary{"dir has .git<br/>or dir is /?"}
  isPastBoundary -->|yes| nothing(["Nothing"])
  isPastBoundary -->|no| isWorkspaceRoot{"Parent is a<br/>workspace root?"}
  isWorkspaceRoot -->|yes| checkParent["Check parent"]
  checkParent -->|found| found(["Detection"])
  isWorkspaceRoot -->|no| repeat(["Repeat with dir = parent"])
  checkParent -->|nothing| repeat
```

"Check" looks at one directory and stops at the first hit, in this order:

1. `packageManager` field in `package.json`
2. `devEngines.packageManager` in `package.json`. When it is a list, only the first entry counts.
3. The files in `lockfilesAndWorkspaceFiles`, in list order
4. The files in `installMetadataFiles`, in list order

A workspace root has a `pnpm-workspace.yaml`, or a `package.json` with a `workspaces` field.

An invalid `package.json` is ignored, and the other files in its directory are still checked.

### Decisions

1. Project files win over the user agent. npm is the default when neither says anything.
2. The search goes no higher than the first directory that has `.git`.
3. Above the start dir, only workspace roots are checked.

### Drawbacks

1. Workspace membership isn't checked. An app under a workspace root gets the root's package manager even when the root's workspace globs don't include it.
