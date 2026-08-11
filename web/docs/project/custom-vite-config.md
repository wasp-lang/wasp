---
title: Custom Vite Config
---

import { ShowForTs, ShowForJs } from '@site/src/components/TsJsHelpers'
import { Optional } from '@site/src/components/Tag'

Wasp uses [Vite](https://vitejs.dev/) to run your app during development and to bundle it for production, both the client and the server. If you want to customize the Vite config, you can do that by editing the `vite.config.{js,ts}` file in your project root directory.

## Required Configuration

You have **full control** over your `vite.config.ts` file. Wasp doesn't manage this file internally. Instead, you must import and use two plugins in your Vite configuration: `wasp()` from `wasp/client/vite` and `waspServer()` from `wasp/server/vite`.

The `wasp()` plugin takes care of the client:

- Configuration required for Wasp full-stack apps to work.
- Environment variables validation.
- Prevention of server imports in client code.
- TypeScript type checking during production builds.

The `waspServer()` plugin takes care of the server:

- Declaration of the [`server` environment](#the-server-environment) Wasp runs and bundles your backend with.
- Running your server inside the Vite dev server during development.
- Prevention of client imports in server code.

Here's the minimal required configuration:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="vite.config.js"
    import { wasp } from 'wasp/client/vite'
    import { waspServer } from 'wasp/server/vite'
    import { defineConfig } from 'vite'

    export default defineConfig({
      plugins: [wasp(), waspServer()],
    })
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="vite.config.ts"
    import { wasp } from 'wasp/client/vite'
    import { waspServer } from 'wasp/server/vite'
    import { defineConfig } from 'vite'

    export default defineConfig({
      plugins: [wasp(), waspServer()],
    })
    ```
  </TabItem>
</Tabs>

:::warning Plugin order
The `wasp()` and `waspServer()` plugins must be the **first** plugins in the `plugins` array. Any other plugins (like Tailwind CSS) should be added after them.
:::

## The Server Environment

The `waspServer()` plugin declares a [Vite environment](https://vite.dev/guide/api-environment) named `server`. Wasp uses it in two places:

- During development, `wasp start` runs a **single Vite process** that serves your client and runs your server. Both print their logs under the same prefix, and Wasp restarts the server in place whenever you change server code. Changing client-only code doesn't restart the server.
- For production, the `npm run bundle` script in `.wasp/out/server` bundles your server through the same environment into `.wasp/out/server/bundle`. Your deployment runs it for you, for example in the Dockerfile `wasp build` generates.

Running `vite build` yourself still means "build the client", it never builds the server.

## Enforced Options

The Wasp plugins enforce certain Vite config values that Wasp needs to function correctly. If you set any of these in your `vite.config.ts`, Wasp will throw an error asking you to remove them.

The `wasp()` plugin enforces:

| Option | Internal value | Why you can't customize it |
|---|---|---|
| `base` | Based on the [`client.baseDir`](./client-config.md#base-directory) option | Wasp sets the React Router's `basename` to the same value. |
| `envPrefix` | `"REACT_APP_"` | Wasp's environment variable validation depends on this prefix. |
| `build.outDir` | `".wasp/out/web-app/build"` | Build artifacts must go to the location Wasp expects for deployment. |

The `waspServer()` plugin enforces:

| Option | Internal value | Why you can't customize it |
|---|---|---|
| `environments.server.build.outDir` | `".wasp/out/server/bundle"` | Wasp and the deployment setup expect the server bundle in that location. |
| `environments.server.build.rolldownOptions.input` | Wasp's generated server entry points | Wasp generates the code that starts your server and runs your database seeds. |

## Customization

You can add additional configuration and plugins as needed. The Wasp plugins will use your config and merge it with the built-in defaults.

Vite config customization can be useful for things like:

- Adding additional Vite plugins.
- Customizing the dev server behavior.
- Customizing the build process.

## Plugin Options

The `wasp()` plugin accepts options allowing you to customize the underlying React plugin behavior if needed:

```ts title="vite.config.ts" auto-js
import { wasp } from 'wasp/client/vite'
import { waspServer } from 'wasp/server/vite'
import { defineConfig } from 'vite'

export default defineConfig({
  plugins: [
    wasp({
      reactOptions: {
        // Pass any @vitejs/plugin-react options here
      }
    }),
    waspServer()
  ],
})
```

## Examples

Below are some examples of how you can customize the Vite config.

### Changing the Dev Server Behaviour

If you want Vite to open the browser automatically when you run `wasp start`, you can do that by customizing the `open` option.

```ts title="vite.config.ts" auto-js
import { wasp } from 'wasp/client/vite'
import { waspServer } from 'wasp/server/vite'
import { defineConfig } from 'vite'

export default defineConfig({
  plugins: [wasp(), waspServer()],
  server: {
    open: true,
  },
})
```

### Custom Dev Server Port

You have access to all of the [Vite dev server options](https://vitejs.dev/config/server-options.html) in your custom Vite config. You can change the **client** dev server port by setting the `port` option. To change the Wasp **server** port, see the [`PORT` server env var](./env-vars.md#server-general-configuration).

```ts title="vite.config.ts" auto-js
import { wasp } from 'wasp/client/vite'
import { waspServer } from 'wasp/server/vite'
import { defineConfig } from 'vite'

export default defineConfig({
  plugins: [wasp(), waspServer()],
  server: {
    port: 4000,
  },
})
```

```env title=".env.server"
WASP_WEB_CLIENT_URL=http://localhost:4000
```

:::warning Changing the client dev server port
Be careful when changing the client dev server port, you'll need to update the `WASP_WEB_CLIENT_URL` env var in your `.env.server` file.
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
import { waspServer } from 'wasp/server/vite'
import { defineConfig } from 'vite'
import devtoolsJson from 'vite-plugin-devtools-json'

export default defineConfig({
  plugins: [
    wasp(),
    waspServer(),
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
import { waspServer } from 'wasp/server/vite'
import { defineConfig } from 'vite'

export default defineConfig({
  plugins: [
    wasp({
      reactOptions: {
        // ...
      },
    }),
    waspServer(),
  ],
})
```

The `wasp()` plugin accepts the following options:

- #### `reactOptions: ReactOptions` <Optional />

  Object to customize the underlying [`@vitejs/plugin-react`](https://github.com/vitejs/vite-plugin-react/tree/main/packages/plugin-react) plugin.

  This allows you to configure React-specific options like Babel plugins, Fast Refresh settings, and JSX configuration.

The `waspServer()` plugin doesn't accept any options.
