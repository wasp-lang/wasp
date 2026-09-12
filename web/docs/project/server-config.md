---
title: Server Config
---

import { CardLink } from "@site/src/components/CardLink";
import { ShowForTs, ShowForJs } from "@site/src/components/TsJsHelpers";

You can configure the behavior of the server via the `server` field of `app` spec:

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"
import { myMiddlewareConfigFn, mySetupFunction } from "./src/myServerSetupCode" with { type: "ref" }

export default app({
  name: "MyApp",
  server: {
    setupFn: mySetupFunction,
    middlewareConfigFn: myMiddlewareConfigFn,
  },
  // ...
})
```

## Setup Function

<ShowForTs>
  `setupFn` declares a Typescript function that will be executed on server start.
</ShowForTs>

<ShowForJs>
  `setupFn` declares a Javascript function that will be executed on server start.
</ShowForJs>

### Adding a Custom Route

As an example, adding a custom route would look something like:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="src/myServerSetupCode.js"
    export const mySetupFunction = async ({ app }) => {
      addCustomRoute(app)
    }

    function addCustomRoute(app) {
      app.get("/customRoute", (_req, res) => {
        res.send("I am a custom route")
      })
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="src/myServerSetupCode.ts"
    import { ServerSetupFn } from "wasp/server"
    import { Application } from "express"

    export const mySetupFunction: ServerSetupFn = async ({ app }) => {
      addCustomRoute(app)
    }

    function addCustomRoute(app: Application) {
      app.get("/customRoute", (_req, res) => {
        res.send("I am a custom route")
      })
    }
    ```
  </TabItem>
</Tabs>

### Storing Some Values for Later Use

In case you want to store some values for later use, or to be accessed by the [Operations](../data-model/operations/overview) you do that in the `setupFn` function.

Dummy example of such function and its usage:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="src/myServerSetupCode.js"
    let someResource = undefined

    export const mySetupFunction = async () => {
      // Let's pretend functions setUpSomeResource and startSomeCronJob
      // are implemented below or imported from another file.
      someResource = await setUpSomeResource()
      startSomeCronJob()
    }

    export const getSomeResource = () => someResource
    ```

    ```js title="src/queries.js"
    import { getSomeResource } from "./myServerSetupCode.js"

    ...

    export const someQuery = async (args, context) => {
      const someResource = getSomeResource()
      return queryDataFromSomeResource(args, someResource)
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="src/myServerSetupCode.ts"
    import { type ServerSetupFn } from "wasp/server"

    let someResource = undefined

    export const mySetupFunction: ServerSetupFn = async () => {
      // Let's pretend functions setUpSomeResource and startSomeCronJob
      // are implemented below or imported from another file.
      someResource = await setUpSomeResource()
      startSomeCronJob()  
    }

    export const getSomeResource = () => someResource
    ```

    ```ts title="src/queries.ts"
    import { type SomeQuery } from "wasp/server/operations"
    import { getSomeResource } from "./myServerSetupCode.js"

    ...

    export const someQuery: SomeQuery<...> = async (args, context) => {
      const someResource = getSomeResource()
      return queryDataFromSomeResource(args, someResource)
    }
    ```
  </TabItem>
</Tabs>

:::note
The recommended way is to put the variable in the same module where you defined the setup function and then expose additional functions for reading those values, which you can then import directly from Operations and use.

This effectively turns your module into a singleton whose construction is performed on server start.
:::

:::note Custom routes and the dev proxy
This depends on your [deployment mode](../deployment/intro.md#deployment-modes):

<Tabs groupId="deployment-mode">
<TabItem value="single" label="Single deployment">

In development the browser talks only to the client dev server, which proxies Wasp's own routes (`/auth`, `/operations`, `/crud`, `/health` and the WebSocket path) and the paths of your declared [`api`s](../advanced/apis.md) to the server. A route you add in `setupFn` (for example `app.get("/customRoute", ...)`) is reachable only on the server port (`http://localhost:3001/customRoute`), not through the app URL. If you want it on the app URL in development too, add a `server.proxy` entry to your `vite.config.ts` that targets `process.env.WASP_DEV_PROXY_TARGET`, the server URL `wasp start` passes to the client:

```ts title="vite.config.ts"
import { defineConfig } from "vitest/config";
import { wasp } from "wasp/client/vite";

export default defineConfig({
  server: {
    proxy: {
      ...(process.env.WASP_DEV_PROXY_TARGET && {
        "/customRoute": process.env.WASP_DEV_PROXY_TARGET,
      }),
    },
  },
  plugins: [wasp()],
});
```

The [kitchen-sink example](https://github.com/wasp-lang/wasp/blob/main/examples/kitchen-sink/vite.config.ts) does this for its `/customRoute`.

In production the server serves both the client and your custom routes, so they are always reachable on the app URL. Keep in mind that a custom route with the same path as one of your pages shadows that page.

</TabItem>
<TabItem value="split" label="Split deployment">

The client and the server are separate origins in development and in production, so there is no proxy: a route you add in `setupFn` is reachable on the server's URL, for example `http://localhost:3001/customRoute` in development. Your pages are served by the client, so a custom route can never shadow one.

</TabItem>
</Tabs>
:::

For the full description of the `setupFn` field, check the [`Server` API Reference](../api/@wasp.sh/spec/interfaces/Server.md#setupfn).

## Middleware Config Function

You can configure the global middleware via the `middlewareConfigFn`. This will modify the middleware stack for all operations and APIs.

Read more in the [configuring middleware section](../advanced/middleware-config#1-customize-global-middleware).

## API Reference

<CardLink
  to="../api/@wasp.sh/spec/interfaces/Server"
  kind="api"
  title="Server"
  description="All the options for the server field of the app spec."
/>
