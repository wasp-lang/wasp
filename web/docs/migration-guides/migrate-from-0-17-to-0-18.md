---
title: Migration from 0.17.X to 0.18.X
---

## What's new in 0.18.0?

### Wasp Tailwind Configuration Now Uses ESM

Wasp has transitioned from CommonJS (CJS) to ECMAScript Modules (ESM) for Tailwind configuration files.
This affects both the **import/export syntax** and **file extensions** (`.cjs` ➝ `.js`).

## How to migrate?

To migrate your Wasp app from 0.16.X to 0.17.X, follow these steps:

### 1. Convert CJS Syntax to ESM

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


### 2. Rename Tailwind Configuration Files

Update the Tailwind configuration files' extensions from `.cjs` to `.js`:
- `tailwind.config.cjs` ➝ `tailwind.config.js`
- `postcss.config.cjs` ➝ `postcss.config.js`

### 3. Check your compatibility with Vite 7

Wasp now uses Vite 7 for better performance and stability. This includes some breaking changes, but we don't expect Wasp apps to be affected by them. If you are using Vite features directly in your app, you should check the migration guides for [Vite 5](https://v5.vite.dev/guide/migration.html), [Vite 6](https://v6.vite.dev/guide/migration.html), and [Vite 7](https://v7.vite.dev/guide/migration.html). We expect most Wasp apps to be unaffected by these changes.

### 4. Enjoy your updated Wasp app

That's it!
