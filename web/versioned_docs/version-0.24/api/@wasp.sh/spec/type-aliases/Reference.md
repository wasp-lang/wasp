# Type Alias: Reference\<AppValue\>

> **Reference**\<`AppValue`\> = [`RefObject`](RefObject.md) \| `AppValue`

Represents a value from your app passed into the Wasp spec.

<!-- Used as a shared snippet in multiple different files with @include helper. -->

Use [reference imports](https://wasp.sh/docs/general/spec#reference-imports)
when passing values from your app into the Wasp spec. If a direct import is not
practical, use [ref](../functions/ref.md) as a fallback.

## Type Parameters

### AppValue

`AppValue`

The expected TypeScript type for the value from your
app. The field using `Reference<AppValue>` decides this type. For example,
[Page.component](../interfaces/Page.md#component) expects a React component, [Query.fn](../interfaces/Query.md#fn) expects a
server function, and env schema fields expect a [ZodSchema](ZodSchema.md).

## Example

```ts
import { app, query } from "@wasp.sh/spec"
import { getTasks } from "./src/queries" with { type: "ref" }

export default app({
  // ...
  spec: [
    query(getTasks, { entities: ["Task"] }),
  ],
})
```
