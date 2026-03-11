---
title: Migration from 0.21.X to 0.22.X
---

import InstallInstructions from './_install-instructions.md'
import LegacyInstallerMigration from './_legacy_installer_migration.md'

<LegacyInstallerMigration />
<InstallInstructions version="0.22" />

## What's new in 0.22.X?

### Upgraded Zod to v4

Wasp now uses Zod v4 for environment variable validation. If you have custom env validation schemas, you may need to update them to be compatible with the latest Zod API. Check the [Zod v4 announcement](https://zod.dev/v4) for details on what changed.

## How to migrate?

### 1. Bump the Wasp version

Update the version field in your Wasp config to `^0.22.0`:

```wasp title="main.wasp"
app MyApp {
  wasp: {
    version: "^0.22.0"
  },
  // ...
}
```

If you're using the TypeScript config:

```ts title="main.wasp.ts"
import { App } from 'wasp-config'

const app = new App('MyApp', {
  wasp: { version: '^0.22.0' },
  // ...
})
```

### 2. Update your env validation schemas (if needed)

If you have custom Zod schemas for env validation, review them for compatibility with Zod v4. Most schemas will work without changes, but some deprecated APIs have been removed. Refer to the [Zod v4 migration guide](https://zod.dev/v4/changelog) for details.
