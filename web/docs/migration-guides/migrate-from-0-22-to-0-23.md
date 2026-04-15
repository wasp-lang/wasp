---
title: Migration from 0.22.X to 0.23.X
---

import InstallInstructions from './_install-instructions.md'
import LegacyInstallerMigration from './_legacy_installer_migration.md'

<LegacyInstallerMigration />
<InstallInstructions version="0.23" />

## How to migrate?

### 1. Upgrade Node.js to 24.14.1 or later

Wasp 0.23 requires Node.js >= 24.14.1 (previously >= 22.22.2). Make sure to upgrade before continuing:

```shell
node -v
# If below 24.14.1, upgrade:
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

### 3. Update TypeScript to 5.9.3

Wasp 0.23 requires TypeScript 5.9.3. Update it in your `package.json`:

<Tabs>
<TabItem value="before" label="Before">

```json title="package.json"
{
  "devDependencies": {
    "typescript": "5.8.2"
  }
}
```

</TabItem>
<TabItem value="after" label="After">

```json title="package.json"
{
  "devDependencies": {
    "typescript": "5.9.3"
  }
}
```

</TabItem>
</Tabs>

### 4. Update your deployment configuration for the new HTML file names

**If you use `wasp deploy` to deploy your app, you can skip this step**

Wasp 0.23 changed the SPA fallback file from `index.html` to `200.html`, in order to support [prerendering](../advanced/prerendering.md).

If you use `wasp deploy` for Fly.io or Railway, this is handled automatically. If you have a custom deployment setup, update your configuration, according to [our updated documentation](../deployment/deployment-methods/overview.md).

In general, you'll have to update any fallback/rewrite rules that point to `index.html`, and point them to `200.html` instead.

### 5. Enjoy your updated Wasp app

That's it!
