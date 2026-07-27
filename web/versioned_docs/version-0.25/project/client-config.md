---
title: Client Config
---

import BaseDirEnvNote from './\_baseDirEnvNote.md'

import { CardLink } from '@site/src/components/CardLink'
import { ShowForTs, ShowForJs } from '@site/src/components/TsJsHelpers'

You can configure the client using the `client` field inside the `app` spec:

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"
import Root from "./src/Root" with { type: "ref" }
import mySetupFunction from "./src/myClientSetupCode" with { type: "ref" }

export default app({
  name: "MyApp",
  client: {
    rootComponent: Root,
    setupFn: mySetupFunction,
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
import { app } from "@wasp.sh/spec"
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
import { app } from "@wasp.sh/spec"
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

For the full description of the `rootComponent` field, check the [`Client` API Reference](../api/@wasp.sh/spec/interfaces/Client.md#rootcomponent).

## Setup Function

`setupFn` declares a <ShowForTs>Typescript</ShowForTs><ShowForJs>JavaScript</ShowForJs> function that Wasp executes on the client before everything else.

:::caution The setup function can also run on the server
The setup function can also run during server-side rendering, like when [prerendering](../advanced/prerendering.md) pages, where browser APIs like `window` are not available. See [Running Code Only on the Client](#running-code-only-on-the-client).
:::

### Running Some Code

We can run any code we want in the setup function.

For example, here's a setup function that logs a message every hour:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="src/myClientSetupCode.js"
    export default async function mySetupFunction() {
      let count = 1
      setInterval(
        () => console.log(`You have been online for ${count++} hours.`),
        1000 * 60 * 60
      )
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="src/myClientSetupCode.ts"
    export default async function mySetupFunction(): Promise<void> {
      let count = 1
      setInterval(
        () => console.log(`You have been online for ${count++} hours.`),
        1000 * 60 * 60
      )
    }
    ```
  </TabItem>
</Tabs>

### Running Code Only on the Client {#running-code-only-on-the-client}

If your app uses [prerendering](../advanced/prerendering.md), Wasp also executes the setup function while rendering your pages on the server. There, browser APIs like `window`, `document`, or `localStorage` don't exist, so using them would crash the prerender. Side effects like timers or event listeners would also run in the Node.js process.

To run some code only in the browser, check Vite's [`import.meta.env.SSR`](https://vite.dev/guide/env-and-mode.html#env-variables) flag, which is `true` during server-side rendering and `false` on the client:

```ts title="src/myClientSetupCode.ts" auto-js
export default async function mySetupFunction(): Promise<void> {
  if (import.meta.env.SSR) {
    // We're rendering on the server, skip the browser-only setup.
    return
  }

  window.addEventListener("online", () => console.log("You are back online!"))
}
```

### Overriding Default Behaviour for Queries

:::info
You can change the options for a **single** Query using the `options` object, as described [here](../data-model/operations/queries#the-usequery-hook-1).
:::

Wasp's `useQuery` hook uses `react-query`'s `useQuery` hook under the hood. Since `react-query` comes configured with aggressive but sane default options, you most likely won't have to change those defaults for all Queries.

If you do need to change the global defaults, you can do so inside the client setup function.

Wasp exposes a `configureQueryClient` hook that lets you configure _react-query_'s `QueryClient` object:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="src/myClientSetupCode.js"
    import { configureQueryClient } from "wasp/client/operations"

    export default async function mySetupFunction() {
      // ... some setup
      configureQueryClient({
        defaultOptions: {
          queries: {
            staleTime: Infinity,
          },
        },
      })
      // ... some more setup
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="src/myClientSetupCode.ts"
    import { configureQueryClient } from "wasp/client/operations"

    export default async function mySetupFunction(): Promise<void> {
      // ... some setup
      configureQueryClient({
        defaultOptions: {
          queries: {
            staleTime: Infinity,
          },
        },
      })
      // ... some more setup
    }
    ```
  </TabItem>
</Tabs>

Make sure to pass in an object expected by the `QueryClient`'s constructor, as
explained in
[react-query's docs](https://tanstack.com/query/v4/docs/reference/QueryClient).

For the full description of the `setupFn` field, check the [`Client` API Reference](../api/@wasp.sh/spec/interfaces/Client.md#setupfn).

## Base Directory

If you need to serve the client from a subdirectory, you can use the `baseDir` option:

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"

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
  to="../api/@wasp.sh/spec/interfaces/Client"
  kind="api"
  title="Client"
  description="All the options for the client field of the app spec."
/>
