---
title: Migration from 0.21.X to 0.22.X
---

import InstallInstructions from './_install-instructions.md'
import LegacyInstallerMigration from './_legacy_installer_migration.md'

<LegacyInstallerMigration />
<InstallInstructions version="0.22" />

## What's new in 0.22.X?

### Node.js minimum version bumped to 22.22.2

Wasp now requires **Node.js 22.22.2 or higher** due to the [March 2026 Node.js security releases](https://nodejs.org/en/blog/vulnerability/march-2026-security-releases). If you're on an older Node.js 22.x version, upgrade before updating Wasp.

### Docker base image upgraded to Alpine 3.23

The generated Dockerfile now uses Alpine 3.23 (previously 3.20). If you have a custom Dockerfile that extends the generated one or depends on Alpine-specific packages, verify compatibility after upgrading.

### Upgraded Zod to v4

Wasp now uses Zod v4 for environment variable validation. If you have custom env validation schemas, you may need to update them to be compatible with the latest Zod API. Check the [Zod v4 announcement](https://zod.dev/v4) for details on what changed.

## How to migrate?

### 1. Upgrade Node.js to 22.22.2 or higher

Make sure you have Node.js 22.22.2 or higher installed. You can check your current version with:

```bash
node -v
```

If you need to upgrade, we recommend using [nvm](https://github.com/nvm-sh/nvm):

```bash
nvm install 22.22.2
```

### 2. Bump the Wasp version

Update the version field in your Wasp config to `^0.22.0`:

```wasp title="main.wasp"
app MyApp {
  wasp: {
    version: "^0.22.0"
  },
  // ...
}
```

### 3. Update your env validation schemas

**If you don't have `app.client.envValidationSchema` or `app.server.envValidationSchema` defined in your Wasp file, you can skip this step.**

Review your schemas for compatibility with Zod v4. Most schemas will work without changes, but some deprecated APIs have been removed. Refer to the [Zod v4 migration guide](https://zod.dev/v4/changelog) for details.
