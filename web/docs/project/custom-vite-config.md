---
title: Custom Vite Config
---

import { ShowForTs, ShowForJs } from '@site/src/components/TsJsHelpers'
import { Optional } from '@site/src/components/Tag'

Wasp uses [Vite](https://vitejs.dev/) to serve the client during development and bundling it for production. If you want to customize the Vite config, you can do that by editing the `vite.config.{js,ts}` file in your project root directory.

## Required Configuration

You have **full control** over your `vite.config.ts` file. Wasp doesn't manage this file internally. Instead, you must import and use the `wasp()` plugin from `wasp/client/vite` in your Vite configuration. This plugin provides all the essential Wasp features:

- Configuration required for Wasp full-stack apps to work.
- Environment variables validation.
- Prevention of server imports in client code.
- TypeScript type checking during production builds.

Here's the minimal required configuration:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="vite.config.js"
    import { wasp } from 'wasp/client/vite'
    import { defineConfig } from 'vite'

    export default defineConfig({
      plugins: [wasp()],
    })
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="vite.config.ts"
    import { wasp } from 'wasp/client/vite'
    import { defineConfig } from 'vite'

    export default defineConfig({
      plugins: [wasp()],
    })
    ```
  </TabItem>
</Tabs>

:::warning Plugin order
The `wasp()` plugin must be the **first** plugin in the `plugins` array. Any other plugins (like Tailwind CSS) should be added after it.
:::

## Enforced Options

The `wasp()` plugin enforces certain Vite config values that Wasp needs to function correctly. If you set any of these in your `vite.config.ts`, Wasp will throw an error asking you to remove them.

| Option | Internal value | Why you can't customize it |
|---|---|---|
| `base` | Based on the [`client.baseDir`](./client-config.md#base-directory) option | Wasp sets the React Router's `basename` to the same value. |
| `envPrefix` | `"REACT_APP_"` | Wasp's environment variable validation depends on this prefix. |
| `build.outDir` | `".wasp/out/web-app/build"` | Build artifacts must go to the location Wasp expects for deployment. |

## Customization

You can add additional configuration and plugins as needed. The `wasp()` plugin will use your config and merge it with the built-in defaults.

Vite config customization can be useful for things like:

- Adding additional Vite plugins.
- Customizing the dev server behavior.
- Customizing the build process.

## Plugin Options

The `wasp()` plugin accepts options allowing you to customize the underlying React plugin behavior if needed:

```ts title="vite.config.ts" auto-js
import { wasp } from 'wasp/client/vite'
import { defineConfig } from 'vite'

export default defineConfig({
  plugins: [
    wasp({
      reactOptions: {
        // Pass any @vitejs/plugin-react options here
      }
    })
  ],
})
```

## Examples

Below are some examples of how you can customize the Vite config.

### Changing the Dev Server Behaviour

If you want to stop Vite from opening the browser automatically when you run `wasp start`, you can do that by customizing the `open` option.

```ts title="vite.config.ts" auto-js
import { wasp } from 'wasp/client/vite'
import { defineConfig } from 'vite'

export default defineConfig({
  plugins: [wasp()],
  server: {
    open: false,
  },
})
```

### Custom Dev Server Port

You have access to all of the [Vite dev server options](https://vitejs.dev/config/server-options.html) in your custom Vite config. You can change the dev server port by setting the `port` option.

```ts title="vite.config.ts" auto-js
import { wasp } from 'wasp/client/vite'
import { defineConfig } from 'vite'

export default defineConfig({
  plugins: [wasp()],
  server: {
    port: 4000,
  },
})
```

```env title=".env.server"
WASP_WEB_CLIENT_URL=http://localhost:4000
```

:::warning Changing the dev server port
Be careful when changing the dev server port, you'll need to update the `WASP_WEB_CLIENT_URL` env var in your `.env.server` file.
:::

### Editing from the Chrome DevTools {#devtools-workspace}

Chrome DevTools support [mapping a page's resources to a folder](https://developer.chrome.com/docs/devtools/workspaces), so any changes you make in the browser are reflected back to your files. To enable it, you can use their Vite plugin: [`vite-plugin-devtools-json`](https://github.com/ChromeDevTools/vite-plugin-devtools-json).

1. Install the plugin as a **dev dependency**:

```bash
npm i -D vite-plugin-devtools-json
```

2. Extend your `vite.config.{ts,js}`:

```ts title="vite.config.ts" auto-js
import { wasp } from 'wasp/client/vite'
import { defineConfig } from 'vite'
import devtoolsJson from 'vite-plugin-devtools-json'

export default defineConfig({
  plugins: [
    wasp(),
    devtoolsJson({ root: import.meta.dirname })
  ]
})
```

3. Start your app with `wasp start`, open **Chrome DevTools → Sources → Workspace** and you should see your project automatically mapped. Changes you make in DevTools now save to disk and Vite's HMR updates the browser instantly!

:::tip Path normalisation
The latest version of `vite-plugin-devtools-json` includes Windows, WSL and Docker Desktop path fixes contributed by the Wasp community – make sure you are on version 0.4.0 or greater.
:::

## API Reference

```ts title="vite.config.ts" auto-js
import { wasp } from 'wasp/client/vite'
import { defineConfig } from 'vite'

export default defineConfig({
  plugins: [
    wasp({
      reactOptions: {
        // ...
      },
    }),
  ],
})
```

The `wasp()` plugin accepts the following options:

- #### `reactOptions: ReactOptions` <Optional />

  Object to customize the underlying [`@vitejs/plugin-react`](https://github.com/vitejs/vite-plugin-react/tree/main/packages/plugin-react) plugin.

  This allows you to configure React-specific options like Babel plugins, Fast Refresh settings, and JSX configuration.
