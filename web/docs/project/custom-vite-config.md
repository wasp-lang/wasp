---
title: Custom Vite Config
---

import { ShowForTs, ShowForJs } from '@site/src/components/TsJsHelpers'

Wasp uses [Vite](https://vitejs.dev/) to serve the client during development and bundling it for production. If you want to customize the Vite config, you can do that by editing the `vite.config.{js,ts}` file in your project root directory.

Wasp will use your config and **merge** it with the default Wasp's Vite config.

Vite config customization can be useful for things like:

- Adding custom Vite plugins.
- Customising the dev server.
- Customising the build process.

Be careful with making changes to the Vite config, as it can break the Wasp's client build process. Check out the default Vite config [here](https://github.com/wasp-lang/wasp/blob/release/waspc/data/Generator/templates/react-app/vite.config.ts) to see what you can change.

## Examples

Below are some examples of how you can customize the Vite config.

### Changing the Dev Server Behaviour

If you want to stop Vite from opening the browser automatically when you run `wasp start`, you can do that by customizing the `open` option.

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="vite.config.js"
    export default {
      server: {
        open: false,
      },
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="vite.config.ts"
    import { defineConfig } from 'vite'

    export default defineConfig({
      server: {
        open: false,
      },
    })
    ```
  </TabItem>
</Tabs>

### Custom Dev Server Port

You have access to all of the [Vite dev server options](https://vitejs.dev/config/server-options.html) in your custom Vite config. You can change the dev server port by setting the `port` option.

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="vite.config.js"
    export default {
      server: {
        port: 4000,
      },
    }
    ```

    ```env title=".env.server"
    WASP_WEB_CLIENT_URL=http://localhost:4000
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="vite.config.ts"
    import { defineConfig } from 'vite'

    export default defineConfig({
      server: {
        port: 4000,
      },
    })
    ```

    ```env title=".env.server"
    WASP_WEB_CLIENT_URL=http://localhost:4000
    ```
  </TabItem>
</Tabs>

:::warning Changing the dev server port
⚠️ Be careful when changing the dev server port, you'll need to update the `WASP_WEB_CLIENT_URL` env var in your `.env.server` file.
:::

### Customising the Base Path

If you, for example, want to serve the client from a different path than `/`, you can do that by customizing the `base` option.

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="vite.config.js"
    export default {
      base: '/my-app/',
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="vite.config.ts"
    import { defineConfig } from 'vite'

    export default defineConfig({
      base: '/my-app/',
    })
    ```
  </TabItem>
</Tabs>

## Chrome DevTools automatic workspace mapping {#devtools-workspace}

Want edits you make in Chrome DevTools Sources panel to save directly to your project files? You can enable [Chrome DevTools Workspace](https://developer.chrome.com/docs/devtools/workspace/) support in development with the community plugin **vite-plugin-devtools-json** (no Wasp fork required).

1. Install the plugin as a **dev-dependency**:

```bash
npm add -D vite-plugin-devtools-json
```

2. Extend your `vite.config.{ts,js}`:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">

```js title="vite.config.js"
import { devtoolsJson } from 'vite-plugin-devtools-json'

export default {
  plugins: [
    react(),              // already present
    devtoolsJson({
      // tell Chrome which folder to map – project root works great
      root: __dirname
    })
  ]
}
```

  </TabItem>
  <TabItem value="ts" label="TypeScript">

```ts title="vite.config.ts"
import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react-swc'
import { devtoolsJson } from 'vite-plugin-devtools-json'

export default defineConfig({
  plugins: [
    react(),
    devtoolsJson({ root: __dirname })
  ]
})
```
  </TabItem>
</Tabs>

3. Start your app with `wasp start`, open Chrome DevTools → **Sources › Filesystems** and you should see your project automatically mapped. Changes you make in DevTools now save to disk and Vite’s HMR updates the browser instantly.

:::tip Path normalisation
The latest version of `vite-plugin-devtools-json` includes Windows, WSL and Docker-desktop path fixes contributed by the Wasp community – make sure you are on ≥ 0.4.0.
:::

This keeps the default Wasp setup lean while letting you opt-in to a powerful workflow with just a few lines in your Vite config.
