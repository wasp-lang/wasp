---
title: Migration from 0.23.X to 0.24.X
---

import InstallInstructions from './_install-instructions.md'
import LegacyInstallerMigration from './_legacy_installer_migration.md'

<LegacyInstallerMigration />
<InstallInstructions version="0.24" />

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

### 2. Enjoy your updated Wasp app

That's it!
