---
title: Migration from 0.21.X to 0.22.X
---

import InstallInstructions from './_install-instructions.md'

<InstallInstructions version="0.22.0" />

## What's new in 0.22.X?

Wasp 0.22 comes with a major overhaul of how the client app is built. User's project directory is now considered the client app directory. This means that Vite is started in user's project directory instead of the `.wasp/out/web-app` directory.

### User-land Vite configuration

You have **full control** over your `vite.config.ts` file. Wasp no longer manages this file internally. Instead, you must import and use the `wasp()` plugin from `wasp/client/vite` in your Vite configuration. This plugin provides all the essential Wasp features:

- Configuration required for Wasp full-stack apps to work
- Environment variable validation
- Prevention of server imports in client code
- TypeScript type checking during production builds

### Simplified Tailwind CSS setup

The `@source(".")` directive is no longer needed in your Tailwind CSS imports. Since Vite is executed in your project directory, Tailwind knows out of the box where to look for your source files.

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

The `wasp()` plugin is **required** and must be the **first plugin** in your Vite configuration. Import it from `wasp/client/vite` and add it to the plugins array:

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

### 3. Remove `@source(".")` from Tailwind CSS imports

**If you're not using Tailwind CSS, skip this step.**

Tailwind now knows how to locate your project's source files automatically. 

Remove the `source(".")` directive from your Tailwind CSS imports:

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

**Skip this step if you don't care about PWA support**

Wasp no longer generates `manifest.json` automatically.

1. Create `manifest.json` in your project's `public/` directory:

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


2. Add a link to the manifest in your head in your Wasp file:

    ```wasp title="main.wasp"
    app TodoApp {
      // ...
      head: [
        // highlight-next-line
        "<link rel='manifest' href='/manifest.json' />",
      ],
    }
    ```

### 5. Clean and reinstall dependencies

To complete the dependency updates, run the following commands in your terminal:

```sh
wasp clean
rm package-lock.json
wasp ts-setup # ONLY if you are using the Wasp TS Config
```

### That's it!

Your Wasp app is now running on 0.22 with user-land Vite configuration.

For more details on customizing your Vite config, check out the [Custom Vite Config documentation](../project/custom-vite-config.md).
