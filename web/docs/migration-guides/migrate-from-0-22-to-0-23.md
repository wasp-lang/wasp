---
title: Migration from 0.22.X to 0.23.X
---

import InstallInstructions from './_install-instructions.md'
import LegacyInstallerMigration from './_legacy_installer_migration.md'

<LegacyInstallerMigration />
<InstallInstructions version="0.23" />

## What's new in 0.23.X?

### Static prerendering support (SSR)

Wasp 0.23 introduces support for static prerendering, which allows you to prerender your app's pages at build time, resulting in faster load times and improved SEO for content-focused pages.

Adding the `prerender: true` to any route will pass that route through Wasp's SSR process, and generate static HTML for it at build time. When a user visits that route, they get the prerendered HTML immediately, and then React hydrates it into a fully interactive app.

You can learn more about this feature in our [prerendering documentation](../advanced/prerendering.md).

### Node.js minimum version bumped to 24.14.1

Wasp now requires **Node.js 24.14.1 or higher**, bundled with npm 11.11.0, so that we can take advantage of the latest features and performance improvements in the Node.js ecosystem.

It also includes an important supply-chain security feature in npm called [`min-release-age`](https://docs.npmjs.com/cli/v11/commands/npm-install#min-release-age). This feature helps protect any npm packages (which can be possibly malicious) from being installed immediately after they are published, giving the community time to respond to any potential security issues.

## How to migrate?

### 1. Upgrade Node.js to 24.14.1 or later

Wasp 0.23 requires Node.js >= 24.14.1 (previously >= 22.22.2). Make sure to upgrade before continuing:

```shell
node -v
# If below 24.14.1, upgrade:
nvm install 24
```

You should also consider enabling the `min-release-age` feature in npm, which will help prevent any supply-chain attacks from malicious npm packages. To do that, add the following to your npm configuration:

```shell
# Will prevent packages newer than 7 days from being installed in this project:
npm config set min-release-age 7

# If you want this to be the default for any project in this machine:
npm config set --global min-release-age 7
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
