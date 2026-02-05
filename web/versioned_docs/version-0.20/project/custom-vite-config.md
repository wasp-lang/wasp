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

### Editing from the Chrome DevTools {#devtools-workspace}

Chrome DevTools support [mapping a page's resources to a folder](https://developer.chrome.com/docs/devtools/workspaces), so any changes you make in the browser are reflected back to your files. To enable it, you can use their Vite plugin: [`vite-plugin-devtools-json`](https://github.com/ChromeDevTools/vite-plugin-devtools-json).

1. Install the plugin as a **dev dependency**:

```bash
npm i -D vite-plugin-devtools-json
```

2. Extend your `vite.config.{ts,js}`:

```ts title="vite.config.ts" auto-js
import { defineConfig } from 'vite'
import devtoolsJson from 'vite-plugin-devtools-json'

export default defineConfig({
  plugins: [
    devtoolsJson({ root: import.meta.dirname })
  ]
})
```

3. Start your app with `wasp start`, open **Chrome DevTools → Sources → Workspace** and you should see your project automatically mapped. Changes you make in DevTools now save to disk and Vite's HMR updates the browser instantly!

:::tip Path normalisation
The latest version of `vite-plugin-devtools-json` includes Windows, WSL and Docker Desktop path fixes contributed by the Wasp community – make sure you are on version 0.4.0 or greater.
:::
