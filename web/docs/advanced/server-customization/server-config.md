---
title: "Server config in Wasp: setupFn, onError and middleware"
sidebar_label: Server config
description: "Configure the Wasp server in the app spec: a setupFn runs on start, for example to add a custom route, onError receives unexpected errors, and middlewareConfigFn changes global middleware."
---

import { CardLink } from "@site/src/components/CardLink";
import { ShowForTs, ShowForJs } from "@site/src/components/TsJsHelpers";

# Server config

You can configure the behavior of the server via the `server` field of `app` spec:

```ts title="main.wasp.ts"
import { app } from "@wasp/spec"
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

In case you want to store some values for later use, or to be accessed by the [Operations](../../features/data/operations/overview) you do that in the `setupFn` function.

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

For the full description of the `setupFn` field, check the [`Server` API Reference](../../api/@wasp/spec.md#setupfn-1).

## Handling Unexpected Errors {#handling-unexpected-errors}

`onError` declares a function that Wasp calls with every unexpected error on the server. Wasp logs the error first, so use the function for anything on top of that, like sending the error to an error tracking service.

```ts title="main.wasp.ts"
import { app } from "@wasp/spec"
import { onServerError } from "./src/serverErrors" with { type: "ref" }

export default app({
  // ...
  server: {
    onError: onServerError,
  },
})
```

The function receives the error and a context whose `source` says where the error happened:

```ts title="src/serverErrors.ts" auto-js
import { type ServerOnErrorFn } from "wasp/server"

export const onServerError: ServerOnErrorFn = (error, context) => {
  if (context.source === "job") {
    console.log(`The ${context.jobName} job failed.`)
  }
  // Let's pretend errorTracker is implemented below or imported from another file.
  errorTracker.capture(error, { source: context.source })
}
```

| `source`      | The error happened...                                               |
| ------------- | ------------------------------------------------------------------- |
| `"request"`   | while handling a request to an Operation, API, or auth route.       |
| `"job"`       | in a [Job](../../features/jobs.md). The context also has `jobName`. |
| `"webSocket"` | while authenticating a WebSocket connection.                        |
| `"oauth"`     | during an OAuth login.                                              |
| `"startup"`   | while starting the server, e.g. in `setupFn`.                       |
| `"dbSeed"`    | in a database seed function, when running `wasp db seed`.           |

A few things to keep in mind:

- An [`HttpError`](../../features/data/operations/queries.md#error-handling) you throw while handling a request is not unexpected, so Wasp doesn't call the function for it.
- The function can be async. The server exits after a `"startup"` or `"dbSeed"` error, and Wasp waits for the function to finish before that. Wasp doesn't wait for it before answering a request.
- If the function itself throws, Wasp logs that error and carries on.

For the full description of the `onError` field, check the [`Server` API Reference](../../api/@wasp/spec.md#onerror-1).

## Middleware Config Function

You can configure the global middleware via the `middlewareConfigFn`. This will modify the middleware stack for all operations and APIs.

Read more in the [configuring middleware section](./middleware#1-customize-global-middleware).

## Working Directory

The server runs with your project's root directory as its working directory, both with `wasp start` and in production. Relative paths in your server code, like `fs.readFile("src/data/countries.json")` or an `uploads/` folder for file uploads, resolve from the project root.

:::caution Don't write files into `src/` at runtime
`wasp start` watches your project's `src/` folder and top-level files. If your server writes there, `wasp start` restarts the server or rebuilds your app, and a server that writes into `src/` on startup gets stuck in a restart loop. Write files to a subfolder of the project root instead, like `uploads/`.
:::

In the Docker image that `wasp build` generates, the project root is `/app`. The image doesn't include your project's files, so if your server reads any of them at runtime, keep them in `src/` and copy them into the image with a [custom Dockerfile](../../deployment/methods/overview.md#customizing-the-dockerfile):

```dockerfile title="Dockerfile"
COPY src/data/ /app/src/data/
```

## Trace Dependencies

The server's Docker image contains only the files Wasp finds by following the server's imports. If a package loads files in a non-standard way, for example static assets it serves from a folder, list it in `traceDeps` so Wasp copies those files too.

Read more in the [deployment section](../../deployment/methods/overview#missing-files-in-the-docker-image).

## API Reference

<CardLink
  to="../../api/@wasp/spec#server-1"
  kind="api"
  title="Server"
  description="All the options for the server field of the app spec."
/>

<CardLink
  to="../../api/wasp/server#serversetupfn"
  kind="api"
  title="ServerSetupFn"
  description="The type of your setupFn function."
/>

<CardLink
  to="../../api/wasp/server#serveronerrorfn"
  kind="api"
  title="ServerOnErrorFn"
  description="The type of your onError function."
/>

<CardLink
  to="../../api/wasp/server#middlewareconfigfn"
  kind="api"
  title="MiddlewareConfigFn"
  description="The type of your middlewareConfigFn function."
/>
