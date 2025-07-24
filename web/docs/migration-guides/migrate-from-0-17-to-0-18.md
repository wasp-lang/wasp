---
title: Migration from 0.17.X to 0.18.X
---

## What's new in 0.18.0?

### Wasp Tailwind Configuration Now Uses ESM

Wasp has transitioned from CommonJS (CJS) to ECMAScript Modules (ESM) for Tailwind configuration files.  
This affects both the **import/export syntax** and **file extensions** (`.cjs` ➝ `.js`).

## How to migrate?

To migrate your Wasp app from 0.16.X to 0.17.X, follow these steps:

### 1. Rename Tailwind Configuration Files

Update the file extensions from `.cjs` to `.js`:
- `tailwind.config.cjs` ➝ `tailwind.config.js`
- `postcss.config.cjs` ➝ `postcss.config.js`

### 2. Convert CJS Syntax to ESM

Update your `tailwind.config.cjs` file to use ESM.

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

```js title="tailwind.config.js"
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
