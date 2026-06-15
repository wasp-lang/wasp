# Interface: Client

Client-side application configuration.

See [Client Config](https://wasp.sh/docs/project/client-config) for usage
details.

## Example

```ts
import { app } from "@wasp.sh/spec"
import Root from "./src/Root" with { type: "ref" }
import mySetupFunction from "./src/myClientSetupCode" with { type: "ref" }

export default app({
  // ...
  client: {
    rootComponent: Root,
    setupFn: mySetupFunction,
    baseDir: "/my-app",
  },
})
```

## Properties

### baseDir?

> `optional` **baseDir?**: `` `/${string}` ``

If you need to serve the client from a subdirectory, you can use the
`baseDir` option.

If you set `baseDir` to `/my-app` for example, that will make Wasp set the
`basename` prop of the `Router` to `/my-app`. It will also set the `base`
option of the Vite config to `/my-app`.

This means that if you serve your app from `https://example.com/my-app`,
the router will work correctly, and all the assets will be served from
`https://example.com/my-app`.

Check the [Base directory
docs](https://wasp.sh/docs/project/client-config#base-directory) for
important details.

***

### envValidationSchema?

> `optional` **envValidationSchema?**: [`Reference`](../type-aliases/Reference.md)\<\{ `_zod`: \{ `def`: `object`; \}; \}\>

Zod schema used to validate user-defined client environment variables at
build time. Wasp merges it with built-in validation for Wasp-defined env
vars when validating `import.meta.env`. Client env vars must be prefixed
with `REACT_APP_`. Define the schema with `defineEnvValidationSchema`
from `wasp/env` to make sure it is type-checked.

<!-- Used as a shared snippet in multiple different files with @include helper. -->

Use [reference imports](https://wasp.sh/docs/general/spec#reference-imports)
when passing values from your app into the Wasp spec. If a direct import is not
practical, use [ref](../functions/ref.md) as a fallback.

For example:

```ts
import { clientEnvSchema } from './src/env' with { type: 'ref' }
```

See [Env Vars](https://wasp.sh/docs/project/env-vars).

***

### rootComponent?

> `optional` **rootComponent?**: [`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

Wrapper React component rendered at the root of the client app. Must
render `Outlet` from `react-router` to show the current page.

Useful for layouts, providers (Redux, theme, etc.), and global UI.

#### Example

```tsx
import { Outlet } from "react-router"
import store from "./store"
import { Provider } from "react-redux"

export default function Root() {
  return (
    <Provider store={store}>
      <Layout />
    </Provider>
  )
}

function Layout() {
  return (
    <div>
      <header>
        <h1>My App</h1>
      </header>

      {/* The current page will be rendered here */}
      <Outlet />

      <footer>
        <p>My App footer</p>
      </footer>
    </div>
  )
}
```

***

### setupFn?

> `optional` **setupFn?**: [`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

Async function Wasp calls once while initializing the client app. Wasp
awaits it before rendering the app. It receives no arguments, and its
return value is ignored. Use it for one-time client-side setup, such as
configuring the query client or starting client-side periodic jobs.

#### Example

```ts
export default async function mySetupFunction(): Promise<void> {
  // Run some code
}
```
