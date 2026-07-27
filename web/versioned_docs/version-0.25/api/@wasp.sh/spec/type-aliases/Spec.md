# Type Alias: Spec

> **Spec** = [`SpecElement`](SpecElement.md) \| `Spec`[]

A single [SpecElement](SpecElement.md), or an (optionally nested) array of them, that
makes up the [App.spec](../interfaces/App.md#spec).

This lets you define related elements separately (for example, one array per
feature) and compose them together. The nested structure is treated as a single
flat list of elements.

## Example

Nested arrays let you compose a spec from separate sub-specs:
```ts
import { app } from "@wasp.sh/spec"

const authSpec: Spec = [signupRoute, loginRoute];
const tasksSpec: Spec = [tasksRoute, getTasks, createTask];

export default app({
  // ...
  spec: [authSpec, tasksSpec],
});
```
