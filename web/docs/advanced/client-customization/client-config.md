---
title: "Client configuration: root component, instrumentation and query client"
sidebar_label: Client configuration
description: "Configure the Wasp client with the client field: a root component for layouts and providers, an instrumentation function, a custom query client and a base directory."
---

import BaseDirEnvNote from './_baseDirEnvNote.md'

import { CardLink } from '@site/src/components/CardLink'
import { ShowForTs, ShowForJs } from '@site/src/components/TsJsHelpers'

# Client configuration

You can configure the client using the `client` field inside the `app` spec:

```ts title="main.wasp.ts"
import { app } from "@wasp/spec"
import Root from "./src/Root" with { type: "ref" }
import { register } from "./src/instrumentation" with { type: "ref" }
import { createQueryClient } from "./src/queryClient" with { type: "ref" }

export default app({
  name: "MyApp",
  client: {
    rootComponent: Root,
    instrumentation: register,
    createQueryClient,
  },
  // ...
})
```

## Root Component

Wasp gives you the option to define a "wrapper" component for your React app.

It can be used for a variety of purposes, but the most common ones are:

- Defining a common layout for your application.
- Setting up various providers that your application needs.

### Defining a Common Layout

Let's define a common layout for your application:

```ts title="main.wasp.ts"
import { app } from "@wasp/spec"
import Root from "./src/Root" with { type: "ref" }

export default app({
  name: "MyApp",
  client: {
    rootComponent: Root,
  },
  // ...
})
```

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```jsx title="src/Root.jsx"
    import { Outlet } from "react-router"

    export default function Root() {
      return (
        <div>
          <header>
            <h1>My App</h1>
          </header>
          // highlight-next-line
          <Outlet />
          <footer>
            <p>My App footer</p>
          </footer>
        </div>
      )
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```tsx title="src/Root.tsx"
    import { Outlet } from "react-router"

    export default function Root() {
      return (
        <div>
          <header>
            <h1>My App</h1>
          </header>
          // highlight-next-line
          <Outlet />
          <footer>
            <p>My App footer</p>
          </footer>
        </div>
      )
    }
    ```
  </TabItem>
</Tabs>

You need to import the [`Outlet`](https://reactrouter.com/8.0.1/api/components/Outlet) component from `react-router` and put it where you want the current page to be rendered.

### Setting up a Provider

This is how to set up various providers that your application needs:

```ts title="main.wasp.ts"
import { app } from "@wasp/spec"
import Root from "./src/Root" with { type: "ref" }

export default app({
  name: "MyApp",
  client: {
    rootComponent: Root,
  },
  // ...
})
```

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```jsx title="src/Root.jsx"
    import { Outlet } from "react-router"
    import store from "./store"
    import { Provider } from "react-redux"

    export default function Root() {
      return (
        <Provider store={store}>
          <Outlet />
        </Provider>
      )
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```tsx title="src/Root.tsx"
    import { Outlet } from "react-router"
    import store from "./store"
    import { Provider } from "react-redux"

    export default function Root() {
      return (
        <Provider store={store}>
          <Outlet />
        </Provider>
      )
    }
    ```
  </TabItem>
</Tabs>

As long as you render the `Outlet` component, you can put what ever you want in the root component.

For the full description of the `rootComponent` field, check the [`Client` API Reference](../../api/@wasp/spec.md#rootcomponent).

## Instrumentation

`instrumentation` declares an async <ShowForTs>TypeScript</ShowForTs><ShowForJs>JavaScript</ShowForJs> function that Wasp calls in the browser before it loads any other client code (your pages, the root component, React, etc.). Wasp waits for it to finish before loading the rest of the app.

Use it to set up tools that must load before the code they observe, such as error-tracking or OpenTelemetry SDKs, or for any other one-time client-side setup.

```ts title="main.wasp.ts"
import { app } from "@wasp/spec"
import { register } from "./src/instrumentation" with { type: "ref" }

export default app({
  name: "MyApp",
  client: {
    instrumentation: register,
  },
  // ...
})
```

For example, here's an instrumentation function that logs a message every hour:

```ts title="src/instrumentation.ts" auto-js
export async function register(): Promise<void> {
  let count = 1
  setInterval(
    () => console.log(`You have been online for ${count++} hours.`),
    1000 * 60 * 60
  )
}
```

Keep in mind:

- Everything the instrumentation module imports also loads before the rest of the app, so only import what the instrumentation needs.
- Wasp only calls the instrumentation in the browser, not while [prerendering](../prerendering.md) pages, so you can use browser APIs like `window` freely.
- Since the app loads after the instrumentation, it may start after the page's `load` event.

For the full description of the `instrumentation` field, check the [`Client` API Reference](../../api/@wasp/spec.md#instrumentation).

## Customizing the Query Client

:::info
You can change the options for a **single** Query using the `options` object, as described in the [`useQuery` API Reference](../../api/wasp/client/operations.md#usequery).
:::

Wasp's `useQuery` hook uses `react-query`'s `useQuery` hook under the hood. Since `react-query` comes configured with aggressive but sane default options, you most likely won't have to change those defaults for all Queries. Wasp adjusts the retry configuration so that Queries failing with a client error, like `401` or `404`, aren't retried, except for `408` and `429` (see [Error Handling](../../features/data/operations/queries.md#error-handling)).

If you need to change the global defaults, or configure other parts of `react-query`'s [`QueryClient`](https://tanstack.com/query/v4/docs/reference/QueryClient) (like its query cache or logger), you can create the `QueryClient` yourself with the `createQueryClient` field:

```ts title="main.wasp.ts"
import { app } from "@wasp/spec"
import { createQueryClient } from "./src/queryClient" with { type: "ref" }

export default app({
  name: "MyApp",
  client: {
    createQueryClient,
  },
  // ...
})
```

Wasp calls the function once, with its default `QueryClient` config as the argument, and uses the `QueryClient` it returns:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="src/queryClient.js"
    import { QueryClient } from "@tanstack/react-query"

    export const createQueryClient = (waspDefaults) =>
      new QueryClient({
        ...waspDefaults,
        defaultOptions: {
          ...waspDefaults.defaultOptions,
          queries: {
            ...waspDefaults.defaultOptions?.queries,
            staleTime: Infinity,
          },
        },
      })
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="src/queryClient.ts"
    import { QueryClient } from "@tanstack/react-query"
    import type { CreateQueryClientFn } from "wasp/client/operations"

    export const createQueryClient: CreateQueryClientFn = (waspDefaults) =>
      new QueryClient({
        ...waspDefaults,
        defaultOptions: {
          ...waspDefaults.defaultOptions,
          queries: {
            ...waspDefaults.defaultOptions?.queries,
            staleTime: Infinity,
          },
        },
      })
    ```
  </TabItem>
</Tabs>

:::caution Keep Wasp's defaults
Pass `waspDefaults` to the `QueryClient` to keep Wasp's default behavior. If you leave them out, Queries go back to `react-query`'s default retry behavior and retry client errors too.
:::

The module that defines `createQueryClient` must not import values from `wasp/client/operations`, because that module needs the query client to load. Type-only imports, like `import type { CreateQueryClientFn }`, are fine.

For the full description of the `createQueryClient` field, check the [`Client` API Reference](../../api/@wasp/spec.md#createqueryclient).

## Base Directory

If you need to serve the client from a subdirectory, you can use the `baseDir` option:

```ts title="main.wasp.ts"
import { app } from "@wasp/spec"

export default app({
  name: "MyApp",
  client: {
    baseDir: "/my-app",
  },
  // ...
})
```

This means that if you serve your app from `https://example.com/my-app`, the
router will work correctly, and all the assets will be served from
`https://example.com/my-app`.

<BaseDirEnvNote />

## API Reference

<CardLink
  to="../../api/@wasp/spec#client-1"
  kind="api"
  title="Client"
  description="All the options for the client field of the app spec."
/>

<CardLink
  to="../../api/wasp/client/operations#createqueryclientfn"
  kind="api"
  title="CreateQueryClientFn"
  description="The type of your createQueryClient function."
/>
