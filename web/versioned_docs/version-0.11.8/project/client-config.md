---
title: Client Config
---

import BaseDirEnvNote from './_baseDirEnvNote.md'

import { ShowForTs, ShowForJs } from '@site/src/components/TsJsHelpers'

You can configure the client using the `client` field inside the `app` declaration:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app MyApp {
  title: "My app",
  // ...
  client: {
    rootComponent: import Root from "@client/Root.jsx",
    setupFn: import mySetupFunction from "@client/myClientSetupCode.js"
  }
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
app MyApp {
  title: "My app",
  // ...
  client: {
    rootComponent: import Root from "@client/Root.tsx",
    setupFn: import mySetupFunction from "@client/myClientSetupCode.ts"
  }
}
```

</TabItem>
</Tabs>

## Root Component

Wasp gives you the option to define a "wrapper" component for your React app.

It can be used for a variety of purposes, but the most common ones are:

- Defining a common layout for your application.
- Setting up various providers that your application needs.

### Defining a Common Layout

Let's define a common layout for your application:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app MyApp {
  title: "My app",
  // ...
  client: {
    rootComponent: import Root from "@client/Root.jsx",
  }
}
```

```jsx title="src/client/Root.jsx"
export default function Root({ children }) {
  return (
    <div>
      <header>
        <h1>My App</h1>
      </header>
      {children}
      <footer>
        <p>My App footer</p>
      </footer>
    </div>
  )
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
app MyApp {
  title: "My app",
  // ...
  client: {
    rootComponent: import Root from "@client/Root.tsx",
  }
}
```

```tsx title="src/client/Root.tsx"
export default function Root({ children }: { children: React.ReactNode }) {
  return (
    <div>
      <header>
        <h1>My App</h1>
      </header>
      {children}
      <footer>
        <p>My App footer</p>
      </footer>
    </div>
  )
}
```

</TabItem>
</Tabs>

### Setting up a Provider

This is how to set up various providers that your application needs:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app MyApp {
  title: "My app",
  // ...
  client: {
    rootComponent: import Root from "@client/Root.jsx",
  }
}
```

```jsx title="src/client/Root.jsx"
import store from './store'
import { Provider } from 'react-redux'

export default function Root({ children }) {
  return <Provider store={store}>{children}</Provider>
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
app MyApp {
  title: "My app",
  // ...
  client: {
    rootComponent: import Root from "@client/Root.tsx",
  }
}
```

```tsx title="src/client/Root.tsx"
import store from './store'
import { Provider } from 'react-redux'

export default function Root({ children }: { children: React.ReactNode }) {
  return <Provider store={store}>{children}</Provider>
}
```

</TabItem>
</Tabs>

As long as you render the children, you can do whatever you want in your root
component.

Read more about the root component in the [API Reference](#rootcomponent-clientimport).

## Setup Function

`setupFn` declares a <ShowForTs>Typescript</ShowForTs><ShowForJs>JavaScript</ShowForJs> function that Wasp executes on the client before everything else.

### Running Some Code

We can run any code we want in the setup function.

For example, here's a setup function that logs a message every hour:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/client/myClientSetupCode.js"
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

```ts title="src/client/myClientSetupCode.ts"
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

### Overriding Default Behaviour for Queries

:::info
You can change the options for a **single** Query using the `options` object, as described [here](/docs/data-model/operations/queries#the-usequery-hook-1).
:::

Wasp's `useQuery` hook uses `react-query`'s `useQuery` hook under the hood. Since `react-query` comes configured with aggressive but sane default options, you most likely won't have to change those defaults for all Queries.

If you do need to change the global defaults, you can do so inside the client setup function.

Wasp exposes a `configureQueryClient` hook that lets you configure _react-query_'s `QueryClient` object:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/client/myClientSetupCode.js"
import { configureQueryClient } from '@wasp/queryClient'

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

```ts title="src/client/myClientSetupCode.ts"
import { configureQueryClient } from '@wasp/queryClient'

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
[react-query's docs](https://tanstack.com/query/v4/docs/react/reference/QueryClient).

Read more about the setup function in the [API Reference](#setupfn-clientimport).

## Base Directory

If you need to serve the client from a subdirectory, you can use the `baseDir` option:

```wasp title="main.wasp"
app MyApp {
  title: "My app",
  // ...
  client: {
    baseDir: "/my-app",
  }
}
```

This means that if you serve your app from `https://example.com/my-app`, the
router will work correctly, and all the assets will be served from
`https://example.com/my-app`.

<BaseDirEnvNote />

## API Reference

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app MyApp {
  title: "My app",
  // ...
  client: {
    rootComponent: import Root from "@client/Root.jsx",
    setupFn: import mySetupFunction from "@client/myClientSetupCode.js"
  }
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
app MyApp {
  title: "My app",
  // ...
  client: {
    rootComponent: import Root from "@client/Root.tsx",
    setupFn: import mySetupFunction from "@client/myClientSetupCode.ts",
    baseDir: "/my-app",
  }
}
```

</TabItem>
</Tabs>

Client has the following options:

- #### `rootComponent: ClientImport`

  `rootComponent` defines the root component of your client application. It is
  expected to be a React component, and Wasp will use it to wrap your entire app.
  It must render its children, which are the actual pages of your application.

  Here's an example of a root component that both sets up a provider and
  renders a custom layout:

  <Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">

  ```jsx title="src/client/Root.jsx"
  import store from './store'
  import { Provider } from 'react-redux'

  export default function Root({ children }) {
    return (
      <Provider store={store}>
        <Layout>{children}</Layout>
      </Provider>
    )
  }

  function Layout({ children }) {
    return (
      <div>
        <header>
          <h1>My App</h1>
        </header>
        {children}
        <footer>
          <p>My App footer</p>
        </footer>
      </div>
    )
  }
  ```

  </TabItem>
  <TabItem value="ts" label="TypeScript">

  ```tsx title="src/client/Root.tsx"
  import store from './store'
  import { Provider } from 'react-redux'

  export default function Root({ children }: { children: React.ReactNode }) {
    return (
      <Provider store={store}>
        <Layout>{children}</Layout>
      </Provider>
    )
  }

  function Layout({ children }: { children: React.ReactNode }) {
    return (
      <div>
        <header>
          <h1>My App</h1>
        </header>
        {children}
        <footer>
          <p>My App footer</p>
        </footer>
      </div>
    )
  }
  ```

  </TabItem>
  </Tabs>

- #### `setupFn: ClientImport`

  <ShowForTs>

  `setupFn` declares a Typescript function that Wasp executes on the client
  before everything else. It is expected to be asynchronous, and
  Wasp will await its completion before rendering the page. The function takes no
  arguments, and its return value is ignored.
  </ShowForTs>

  <ShowForJs>

  `setupFn` declares a JavaScript function that Wasp executes on the client
  before everything else. It is expected to be asynchronous, and
  Wasp will await its completion before rendering the page. The function takes no
  arguments, and its return value is ignored.
  </ShowForJs>

  You can use this function to perform any custom setup (e.g., setting up
  client-side periodic jobs).

  <Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">

  ```js title="src/client/myClientSetupCode.js"
  export default async function mySetupFunction() {
    // Run some code
  }
  ```

  </TabItem>
  <TabItem value="ts" label="TypeScript">

  ```ts title="src/client/myClientSetupCode.ts"
  export default async function mySetupFunction(): Promise<void> {
    // Run some code
  }
  ```

  </TabItem>
  </Tabs>

- #### `baseDir: String`

  If you need to serve the client from a subdirectory, you can use the `baseDir` option.

  If you set `baseDir` to `/my-app` for example, that will make Wasp set the `basename` prop of the `Router` to
  `/my-app`. It will also set the `base` option of the Vite config to `/my-app`.

  This means that if you serve your app from `https://example.com/my-app`, the router will work correctly, and all the assets will be served from `https://example.com/my-app`.

  <BaseDirEnvNote />
