# Function: query()

> **query**(`fn`, `config?`): [`Query`](../interfaces/Query.md)

Creates a [Query](../interfaces/Query.md) definition.

Queries are server-side read-only operations. They can be invoked from
the client (with caching via `useQuery`) and the server. List the
entities the query reads from so Wasp can inject the matching Prisma
delegates into `context.entities` and invalidate the client cache when
related actions modify them.

See [Queries](https://wasp.sh/docs/data-model/operations/queries).

## Parameters

### fn

[`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

Reference to the Query's NodeJS implementation.

See [the
docs](https://wasp.sh/docs/data-model/operations/queries#implementing-queries)
for details on the implementation and its context.

<!-- Used as a shared snippet in multiple different files with @include helper. -->

Use [reference imports](https://wasp.sh/docs/general/spec#reference-imports)
when passing values from your app into the Wasp spec. If a direct import is not
practical, use [ref](ref.md) as a fallback.

### config?

#### auth?

`boolean`

Whether this Query requires auth. If your app has auth enabled, this
defaults to `true`.

Only available if your app has auth enabled.

#### entities?

`string`[]

A list of entities you wish to use inside your Query.

See [Using Entities in Queries](https://wasp.sh/docs/data-model/operations/queries#using-entities-in-queries).

## Returns

[`Query`](../interfaces/Query.md)

## Example

```ts
import { app, query } from "@wasp.sh/spec"
import { getTasks } from './src/queries' with { type: 'ref' }

export default app({
  // ...
  spec: [
    query(getTasks, { entities: ["Foo"] }),
  ],
})
```
