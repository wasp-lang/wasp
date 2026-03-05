---
title: Migration from 0.21.X to 0.22.X
---

import InstallInstructions from './_install-instructions.md'
import LegacyInstallerMigration from './_legacy_installer_migration.md'

<LegacyInstallerMigration />

<InstallInstructions version="0.22" />

## What's new in 0.22.X?

### Vitest is now user-land

Wasp no longer bundles Vitest or any testing dependencies. This gives you full control over your test setup: choose your own versions, configure Vitest directly in `vite.config.ts`, and write both client and server-side tests.

`wasp/client/test` now exports only `WaspTestWrapper`. The `renderInContext` and `mockServer` helpers have been removed from the SDK.

The `wasp test client` CLI command has been removed. Run `npx vitest` from your project root instead.

## How to migrate?

### 1. Bump the Wasp version

Update the version field in your Wasp file to `^0.22.0`:

```wasp title="main.wasp"
app MyApp {
  wasp: {
    // highlight-next-line
    version: "^0.22.0"
  },
}
```

### 2. Set up Vitest

**If you don't have test files in your project, you can skip this step.**

Follow the [Vitest setup guide](../guides/libraries/vitest.md) to install dependencies, configure `vite.config.ts`, and set up your test helpers.

### 3. Replace `wasp test client` with `npx vitest`

**If you weren't using `wasp test client`, you can skip this step.**

```bash
# Before
wasp test client

# After
npx vitest
```

### 4. Enjoy your updated Wasp app

That's it!
