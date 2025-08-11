---
title: Migration from 0.17.X to 0.18.X
---

## What's new in 0.18.0?

### Wasp now requires Node.js >=22.12

We've updated our Node.js version requirement to **Node.js 22.12 or higher**, ahead of [the upcoming LTS releases in October 2025](https://github.com/nodejs/Release/blob/755d5821ca9454b91d83f51736b4dddbd7a2600c/README.md).

The jump from Node.js 20 to 22 brings significant [performance improvements](https://nodejs.org/en/blog/announcements/v21-release-announce#performance), new features (like [stable `fetch`](https://nodejs.org/en/blog/announcements/v21-release-announce#stable-fetchwebstreams) or [`require(esm)`](https://nodejs.org/en/blog/release/v22.12.0#requireesm-is-now-enabled-by-default)), and overall enhanced security.

These releases are light on breaking changes and we expect the vast majority (if not all) of Wasp apps to run on the new version, unchanged.

### Wasp now uses Vite 7

Wasp has upgraded to Vite 7 internally, which brings performance improvements and improved compatibility. You can now also use newer plugins in your Vite configuration that take advantage of Vite 7 features.

This upgrade contains no known breaking changes for Wasp apps and we expect most of them to upgrade without any code changes.

### Wasp Tailwind Configuration Now Uses ESM

Wasp has transitioned from CommonJS (CJS) to ECMAScript Modules (ESM) for Tailwind configuration files.
This affects both the **import/export syntax** and **file extensions** (`.cjs` ➝ `.js`).

## How to migrate?

To migrate your Wasp app from 0.16.X to 0.17.X, follow these steps:

### 1. Install Node.js 22.12 or higher

Make sure you have Node.js 22.12 or higher installed. You can check your current version with:

```bash
node -v
```

If you followed our [Quick Start tutorial](../quick-start#requirements), you can use `nvm use 22` to upgrade your Node.js version.
If you installed Node.js some other way, you can check their [official installation guide](https://nodejs.org/en/download/) for more guidance. 

### 2. Convert CJS Syntax to ESM

Update your `tailwind.config.cjs` file to use ESM:

<Tabs>
<TabItem value="before" label="Before">

```js title="tailwind.config.cjs"
const { resolveProjectPath } = require('wasp/dev')

/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [resolveProjectPath("./src/**/*.{js,jsx,ts,tsx}")],
  theme: {
    extend: {},
  },
  plugins: [require('@tailwindcss/typography')],
};
```

</TabItem>
<TabItem value="after" label="After">

```js title="tailwind.config.cjs"
import TailwindTypography from "@tailwindcss/typography";
import { resolveProjectPath } from "wasp/dev";

/** @type {import('tailwindcss').Config} */
export default {
  content: [resolveProjectPath("./src/**/*.{js,jsx,ts,tsx}")],
  theme: {
    extend: {},
  },
  plugins: [TailwindTypography],
};
```

</TabItem>
</Tabs>

Same for the `postcss.config.cjs` file:

<Tabs>
<TabItem value="before" label="Before">

```js title="postcss.config.cjs"
module.exports = {
  plugins: {
    tailwindcss: {},
    autoprefixer: {},
  },
};
```

</TabItem>
<TabItem value="after" label="After">

```js title="postcss.config.cjs"
export default {
  plugins: {
    tailwindcss: {},
    autoprefixer: {},
  },
};
```

</TabItem>
</Tabs>


### 3. Rename Tailwind Configuration Files

Update the Tailwind configuration files' extensions from `.cjs` to `.js`:
- `tailwind.config.cjs` ➝ `tailwind.config.js`
- `postcss.config.cjs` ➝ `postcss.config.js`

### 4. Check your compatibility with Vite 7

Wasp now uses Vite 7 for better performance and stability. This includes some breaking changes, but we don't expect Wasp apps to be affected by them. If you are using Vite features directly in your app, you should check the migration guides for [v5](https://v5.vite.dev/guide/migration.html), [v6](https://v6.vite.dev/guide/migration.html), and [v7](https://v7.vite.dev/guide/migration.html). We expect most Wasp apps to be unaffected by these changes.

The only manual change you need to make is to update your `package.json` file:

<Tabs>
<TabItem value="before" label="Before">

```json title="package.json"
{
  // ...
  "devDependencies": {
    // ...
    "vite": "^4.3.9"
  }
}
```

</TabItem>
<TabItem value="after" label="After">

```json title="package.json"
{
  // ...
  "devDependencies": {
    // ...
    "vite": "^7.0.6"
  }
}
```

</TabItem>
</Tabs>

### 5. Enjoy your updated Wasp app

That's it!
