---
title: Migration from 0.23.X to 0.24.X
---

import InstallInstructions from './_install-instructions.md'
import LegacyInstallerMigration from './_legacy_installer_migration.md'

<LegacyInstallerMigration />
<InstallInstructions version="0.22" />

## What's new in 0.24.X?

## How to migrate?

### 1. Bump the Wasp version

Update the version field in your Wasp config to `^0.24.0`:

```wasp title="main.wasp"
app MyApp {
  wasp: {
    version: "^0.24.0"
  },
  // ...
}
```

### 2. Update your `tsconfig.json` / `tsconfig.src.json` files=

Due to internal `wasp/sdk` package changes, we require a simple change to your `tsconfig.json` or `tsconfig.src.json` if you are using the Wasp TS configuration.

Update the `include` field of the TypeScript configuration:

```json title="tsconfig.json"
{
  "compilerOptions": {
    // ...
    "outDir": ".wasp/out/user"
  },
  // highlight-next-line
  "include": ["src", ".wasp/out/types"]
}

```
