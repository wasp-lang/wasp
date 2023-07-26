---
title: Server Config
---

Via `server` field of `app` declaration, you can configure the behavior of the Node.js server (one that is executing wasp operations).

```wasp
app MyApp {
  title: "My app",
  // ...
  server: {
    setupFn: import { mySetupFunction } from "@server/myServerSetupCode.js",
    middlewareConfigFn: import { myMiddlewareConfigFn } from "@server/myServerSetupCode.js"
  }
}
```

`app.server` is a dictionary with the following fields:

#### `middlewareConfigFn: ServerImport` (optional)

The import statement to an Express middleware config function. This is a global modification affecting all operations and APIs. See [Configuring Middleware](/docs/advanced/middleware-config). <!-- TODO: Put more precise link here -->

#### `setupFn: ServerImport` (optional)

`setupFn` declares a JS function that will be executed on server start. This function is expected to be async and will be awaited before the server starts accepting any requests.

It allows you to do any custom setup, e.g. setting up additional database/websockets or starting cron/scheduled jobs.

The `setupFn` function receives the `express.Application` and the `http.Server` instances as part of its context. They can be useful for setting up any custom server routes or for example, setting up `socket.io`.
```ts
export type ServerSetupFn = (context: ServerSetupFnContext) => Promise<void>

export type ServerSetupFnContext = {
  app: Application, // === express.Application
  server: Server,   // === http.Server
}
```

As an example, adding a custom route would look something like:
```ts title="src/server/myServerSetupCode.ts"
import { ServerSetupFn, Application } from '@wasp/types'

export const mySetupFunction: ServerSetupFn = async ({ app }) => {
  addCustomRoute(app)
}

function addCustomRoute(app: Application) {
  app.get('/customRoute', (_req, res) => {
    res.send('I am a custom route')
  })
}
```

In case you want to store some values for later use, or to be accessed by the Operations, recommended way is to store those in variables in the same module/file where you defined the javascript setup function and then expose additional functions for reading those values, which you can then import directly from Operations and use. This effectively turns your module into a singleton whose construction is performed on server start.

Dummy example of such function and its usage:

```js title="src/server/myServerSetupCode.js"
let someResource = undefined

export const mySetupFunction = async () => {
  // Let's pretend functions setUpSomeResource and startSomeCronJob
  // are implemented below or imported from another file.
  someResource = await setUpSomeResource()
  startSomeCronJob()
}

export const getSomeResource = () => someResource
```

```js title="src/server/queries.js"
import { getSomeResource } from './myServerSetupCode.js'

...

export const someQuery = async (args, context) => {
  const someResource = getSomeResource()
  return queryDataFromSomeResource(args, someResource)
}
```
