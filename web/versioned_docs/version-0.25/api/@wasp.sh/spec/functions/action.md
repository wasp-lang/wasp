# Function: action()

> **action**(`fn`, `config?`): [`Action`](../interfaces/Action.md)

Creates an [Action](../interfaces/Action.md) definition.

Actions are server-side write operations. Like queries, they can be
called from the client and the server. Listing entities in `config.entities`
lets Wasp invalidate related query caches when this action runs.

See [Actions](https://wasp.sh/docs/data-model/operations/actions).

## Parameters

### fn

[`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

Reference to the Action's NodeJS implementation.

See [the docs](https://wasp.sh/docs/data-model/operations/actions#implementing-actions) for details on the implementation and its context.

<!-- Used as a shared snippet in multiple different files with @include helper. -->

Use [reference imports](https://wasp.sh/docs/general/spec#reference-imports)
when passing values from your app into the Wasp spec. If a direct import is not
practical, use [ref](ref.md) as a fallback.

### config?

#### auth?

`boolean`

Whether this Action requires auth. If your app has auth enabled, this
defaults to `true`.

Only available if your app has auth enabled.

#### entities?

`string`[]

A list of entities you wish to use inside your Action.

See [Using Entities in Actions](https://wasp.sh/docs/data-model/operations/actions#using-entities-in-actions).

## Returns

[`Action`](../interfaces/Action.md)

## Example

```ts
import { app, action } from "@wasp.sh/spec"
import { createTask } from "./src/actions" with { type: "ref" }
export default app({
  // ...
  spec: [
    action(createTask, { entities: ["Task"] }),
  ],
})
```
