---
title: Migration from 0.21.X to 0.22.X
---

import InstallInstructions from './_install-instructions.md'

<InstallInstructions version="0.22.0" />

## What's new in 0.22.0?

Wasp 0.22 brings major changes to how the frontend build is configured. You now own your `vite.config.ts` file and must add the `wasp()` plugin to it. This gives you full control over your Vite configuration while the plugin handles Wasp-specific features.

### User-land Vite configuration

Starting from Wasp 0.22, you have **full control** over your `vite.config.ts` file. Wasp no longer manages this file internally. Instead, you must import and use the `wasp()` plugin from `wasp/client/vite` in your Vite configuration. This plugin provides all the essential Wasp features:

- Automatic Tailwind CSS content scanning (no `@source(".")` needed)
- Virtual entry points (`index.tsx`, `routes.tsx`)
- Environment variable validation
- Prevention of server imports in client code
- TypeScript type checking during production builds

### Simplified Tailwind CSS setup

The `@source(".")` directive is no longer needed in your Tailwind CSS imports. The `wasp()` plugin now handles content scanning automatically. This simplifies your CSS setup and eliminates a common source of confusion.

## How to migrate?

To migrate your Wasp app from 0.21.X to 0.22.X, follow these steps:

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

### 2. Add the `wasp()` plugin to your `vite.config.ts`

The `wasp()` plugin is now **mandatory** and must be the **first plugin** in your Vite configuration. Import it from `wasp/client/vite` and add it to the plugins array:

<Tabs>
<TabItem value="before" label="Before">

```ts title="vite.config.ts"
import tailwindcss from '@tailwindcss/vite'
import { defineConfig } from 'vite'

export default defineConfig({
  plugins: [tailwindcss()],
  server: {
    open: true,
  },
})
```

</TabItem>
<TabItem value="after" label="After">

```ts title="vite.config.ts"
// highlight-next-line
import { wasp } from 'wasp/client/vite'
import tailwindcss from '@tailwindcss/vite'
import { defineConfig } from 'vite'

export default defineConfig({
  plugins: [
    // highlight-next-line
    wasp(),
    tailwindcss()
  ],
  server: {
    open: true,
  },
})
```

</TabItem>
</Tabs>

:::warning Plugin order is important
The `wasp()` plugin **must be the first plugin** in the `plugins` array. Any other plugins like Tailwind CSS or others must come after it.
:::

### 3. Remove `@source(".")` from Tailwind CSS imports

**If you're not using Tailwind CSS, skip this step.**

The `wasp()` plugin now handles content scanning automatically. Remove the `source(".")` directive from your Tailwind CSS imports:

<Tabs>
<TabItem value="before" label="Before">

```css title="src/Main.css"
// highlight-next-line
@import "tailwindcss" source(".");

/* ... */
```

</TabItem>
<TabItem value="after" label="After">

```css title="src/Main.css"
// highlight-next-line
@import "tailwindcss";

/* ... */
```

</TabItem>
</Tabs>

### 4. Create `public/manifest.json`

Wasp no longer generates `manifest.json` automatically. Create one in your project's `public/` directory:

```json title="public/manifest.json"
{
  "name": "MyAwesomeApp",
  "icons": [
    {
      "src": "favicon.ico",
      "sizes": "64x64 32x32 24x24 16x16",
      "type": "image/x-icon"
    }
  ],
  "start_url": ".",
  "display": "standalone",
  "theme_color": "#000000",
  "background_color": "#ffffff"
}
```

Make sure to customize the `name`, `theme_color`, and `background_color` to match your app.

### 5. Clean and reinstall dependencies

After making the above changes, clean up old build artifacts and reinstall dependencies:

```sh
wasp clean
rm -rf node_modules package-lock.json
wasp start
```

### 6. Test your app

Start your Wasp app and verify everything works correctly:

```sh
wasp start
```

### That's it!

Your Wasp app is now running on 0.22 with user-land Vite configuration.

For more details on customizing your Vite config, check out the [Custom Vite Config documentation](../project/custom-vite-config.md).
