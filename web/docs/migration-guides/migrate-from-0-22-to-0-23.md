---
title: Migration from 0.22.X to 0.23.X
---

import InstallInstructions from './_install-instructions.md'
import LegacyInstallerMigration from './_legacy_installer_migration.md'

<LegacyInstallerMigration />
<InstallInstructions version="0.23" />

## What's new in 0.23.X?

## How to migrate?

### 1. Upgrade Node.js to v24.14.1 or later

Wasp 0.23 requires Node.js >= v24.14.1 (previously >= v22.22.2). Make sure to upgrade before continuing:

```shell
node -v
# If below v24.14.1, upgrade:
nvm install 24
```

### 2. Bump the Wasp version

Update the version field in your Wasp config to `^0.23.0`:

```wasp title="main.wasp"
app MyApp {
  wasp: {
    version: "^0.23.0"
  },
  // ...
}
```
