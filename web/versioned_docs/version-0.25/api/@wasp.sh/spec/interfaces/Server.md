# Interface: Server

Server-side application configuration.

See [Server Config](https://wasp.sh/docs/project/server-config) for usage
details.

## Example

```ts
import { app } from "@wasp.sh/spec"
import { myMiddlewareConfigFn, mySetupFunction } from "./src/myServerSetupCode" with { type: "ref" }

export default app({
  // ...
  server: {
    setupFn: mySetupFunction,
    middlewareConfigFn: myMiddlewareConfigFn,
  },
})
```

## Properties

### envValidationSchema?

> `optional` **envValidationSchema?**: [`Reference`](../type-aliases/Reference.md)\<\{ `_zod`: \{ `def`: `object`; \}; \}\>

Zod schema used to validate user-defined server environment variables on
startup. Wasp merges it with built-in validation for Wasp-defined env vars
when validating `process.env`. Define the schema with
`defineEnvValidationSchema` from `wasp/env` to make sure it is
type-checked.

<!-- Used as a shared snippet in multiple different files with @include helper. -->

Use [reference imports](https://wasp.sh/docs/general/spec#reference-imports)
when passing values from your app into the Wasp spec. If a direct import is not
practical, use [ref](../functions/ref.md) as a fallback.

For example:

```ts
import { serverEnvSchema } from './src/env' with { type: 'ref' }
```

See [Env Vars](https://wasp.sh/docs/project/env-vars).

***

### middlewareConfigFn?

> `optional` **middlewareConfigFn?**: [`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

Function that customizes the global Express middleware stack. Affects
all operations and APIs.

See [Configuring Middleware](https://wasp.sh/docs/advanced/middleware-config).

***

### setupFn?

> `optional` **setupFn?**: [`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

Async function called once on server start. Wasp awaits it before the
server accepts requests. Receives a context containing the Express `app`
and underlying `http.Server`, so you can register custom routes, set up
additional databases or WebSockets, or kick off scheduled jobs.

In TypeScript, you can type the function with the `ServerSetupFn` type
from `wasp/server`.

#### Example

```ts
import { type ServerSetupFn } from "wasp/server"

export const mySetupFunction: ServerSetupFn = async () => {
  await setUpSomeResource()
}
```
