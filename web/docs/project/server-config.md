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
    basePath: "/api",
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

:::note
`setupFn` receives the Express app mounted at the [base path](#base-path), so a route you add there lives under it like everything else on the server: with `basePath: "/api"`, `app.get("/customRoute", ...)` answers on `http://localhost:3001/api/customRoute`.
:::

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

For the full description of the `setupFn` field, check the [`Server` API Reference](../api/@wasp.sh/spec/interfaces/Server.md#setupfn).

## Middleware Config Function

You can configure the global middleware via the `middlewareConfigFn`. This will modify the middleware stack for all operations and APIs.

Read more in the [configuring middleware section](../advanced/middleware-config#1-customize-global-middleware).

## Base Path

The `basePath` option is the path under which the whole server lives: Wasp's own routes (auth, operations and CRUD), your [`api`](../advanced/apis.md) and `apiNamespace` routes, and anything you add in [`setupFn`](#setup-function):

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"

export default app({
  name: "MyApp",
  server: {
    basePath: "/api",
  },
  // ...
})
```

With `basePath: "/api"`, the server answers on `/api/auth/*`, `/api/operations/*` and `/api/crud/*`, and an `api` declared at `/foo/bar` is served at `/api/foo/bar`. New Wasp projects come with `/api`. If you leave the option out, the base path is `/` and the server's routes sit at the server's root, as before.

The base path is an absolute path on the server's origin. It must start with `/` and must not end with `/` (except for `/` itself).

A few things follow from it:

- Wasp derives every server URL from it. The OAuth redirect URI you register with a provider is `<server origin><basePath>/auth/<provider>/callback`, for example `https://api.myapp.com/api/auth/google/callback`. `WASP_SERVER_URL` and `REACT_APP_API_URL` stay the server's origin, without the base path: Wasp appends it itself.
- Your own [`api`](../advanced/apis.md) and `apiNamespace` paths are declared relative to it and served under it. Wasp's own routes are mounted first, so an `api` at exactly one of Wasp's paths (`/auth/...`, `/operations/<name>`, `/crud/<name>/...`) is shadowed by them; other paths under those prefixes work. An endpoint whose path is dictated from the outside (a `/.well-known/...` file, say) can [opt out of the base path](../advanced/apis.md#opting-an-endpoint-out-of-the-base-path) with `ignoreServerBasePath: true`.
- On the client, `config.apiUrl` from `wasp/client` is the server origin plus the base path, and the `api` instance from `wasp/client/api` is rooted there: `api.get("/foo/bar")` hits `/api/foo/bar`.
- The server root (`GET /`) stays outside the base path: in development it shows a page pointing to the client, in production it returns an empty `200`.
- Server code can read it as `config.serverBasePath` from `wasp/server`.

## API Reference

<CardLink
  to="../api/@wasp.sh/spec/interfaces/Server"
  kind="api"
  title="Server"
  description="All the options for the server field of the app spec."
/>
