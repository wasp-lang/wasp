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

### 2. Update your env validation schemas

**If you don't have `app.client.envValidationSchema` or `app.server.envValidationSchema` defined in your Wasp file, you can skip this step.**

Review your schemas for compatibility with Zod v4. Most schemas will work without changes, but some deprecated APIs have been removed. Refer to the [Zod v4 migration guide](https://zod.dev/v4/changelog) for details.

### 3. Update your `app.head` tags to be valid JSX

**If you don't have `app.head` defined in your Wasp file, you can skip this step.**

We now use React to output the base `index.html` for your client app, as we set the groundwork for SSR support in the future. This means that the contents of `app.head` are now rendered as React JSX instead of raw HTML.

To make sure all tags in `app.head` are valid React JSX, check that every tag is either self-closing (e.g. `<meta ... />`) or has a matching closing tag (e.g. `<script>...</script>`). In JSX, even void HTML elements (like `<link>`, `<meta>`, and `<base>`) need a trailing `/>`.

Wasp will print an error on compilation if it encounters any invalid JSX in `app.head`, so you can use that as a guide to fix any issues.
