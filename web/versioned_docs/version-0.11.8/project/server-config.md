---
title: Server Config
---

import { ShowForTs, ShowForJs } from "@site/src/components/TsJsHelpers";

You can configure the behavior of the server via the `server` field of `app` declaration:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app MyApp {
  title: "My app",
  // ...
  server: {
    setupFn: import { mySetupFunction } from "@server/myServerSetupCode.js",
    middlewareConfigFn: import { myMiddlewareConfigFn } from "@server/myServerSetupCode.js"
  }
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
app MyApp {
  title: "My app",
  // ...
  server: {
    setupFn: import { mySetupFunction } from "@server/myServerSetupCode.js",
    middlewareConfigFn: import { myMiddlewareConfigFn } from "@server/myServerSetupCode.js"
  }
}
```

</TabItem>
</Tabs>

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

```js title="src/server/myServerSetupCode.ts"
export const mySetupFunction = async ({ app }) => {
  addCustomRoute(app)
}

function addCustomRoute(app) {
  app.get('/customRoute', (_req, res) => {
    res.send('I am a custom route')
  })
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

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

</TabItem>
</Tabs>

### Storing Some Values for Later Use

In case you want to store some values for later use, or to be accessed by the [Operations](/docs/data-model/operations/overview) you do that in the `setupFn` function.

Dummy example of such function and its usage:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

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

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/server/myServerSetupCode.ts"
import { ServerSetupFn } from '@wasp/types'

let someResource = undefined

export const mySetupFunction: ServerSetupFn = async () => {
  // Let's pretend functions setUpSomeResource and startSomeCronJob
  // are implemented below or imported from another file.
  someResource = await setUpSomeResource()
  startSomeCronJob()
}

export const getSomeResource = () => someResource
```

```ts title="src/server/queries.ts"
import { SomeQuery } from '@wasp/queries/types'
import { getSomeResource } from './myServerSetupCode.js'

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

Read more about [server setup function](#setupfn-serverimport) below.

## Middleware Config Function

You can configure the global middleware via the `middlewareConfigFn`. This will modify the middleware stack for all operations and APIs.

Read more about [middleware config function](#middlewareconfigfn-serverimport) below.

## API Reference

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app MyApp {
  title: "My app",
  // ...
  server: {
    setupFn: import { mySetupFunction } from "@server/myServerSetupCode.js",
    middlewareConfigFn: import { myMiddlewareConfigFn } from "@server/myServerSetupCode.js"
  }
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
app MyApp {
  title: "My app",
  // ...
  server: {
    setupFn: import { mySetupFunction } from "@server/myServerSetupCode.js",
    middlewareConfigFn: import { myMiddlewareConfigFn } from "@server/myServerSetupCode.js"
  }
}
```

</TabItem>
</Tabs>

`app.server` is a dictionary with the following fields:

- #### `setupFn: ServerImport`

  `setupFn` declares a <ShowForTs>Typescript</ShowForTs><ShowForJs>Javascript</ShowForJs> function that will be executed on server start. This function is expected to be async and will be awaited before the server starts accepting any requests.

  It allows you to do any custom setup, e.g. setting up additional database/websockets or starting cron/scheduled jobs.

  The `setupFn` function receives the `express.Application` and the `http.Server` instances as part of its context. They can be useful for setting up any custom server routes or for example, setting up `socket.io`.

  <Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">

  ```js title="src/server/myServerSetupCode.js"
  export const mySetupFunction = async () => {
    await setUpSomeResource()
  }
  ```

  </TabItem>
  <TabItem value="ts" label="TypeScript">

  Types for the setup function and its context are as follows:

  ```ts title="@wasp/types"
  export type ServerSetupFn = (context: ServerSetupFnContext) => Promise<void>

  export type ServerSetupFnContext = {
    app: Application // === express.Application
    server: Server // === http.Server
  }
  ```

  ```ts title="src/server/myServerSetupCode.ts"
  import { ServerSetupFn } from '@wasp/types'

  export const mySetupFunction: ServerSetupFn = async () => {
    await setUpSomeResource()
  }
  ```

  </TabItem>
  </Tabs>

- #### `middlewareConfigFn: ServerImport`

  The import statement to an Express middleware config function. This is a global modification affecting all operations and APIs. See more in the [configuring middleware section](/docs/advanced/middleware-config#1-customize-global-middleware).
