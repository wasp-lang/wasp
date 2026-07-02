# Function: crud()

> **crud**(`name`, `entity`, `operations`): [`Crud`](../interfaces/Crud.md)

Creates a [Crud](../interfaces/Crud.md) definition.

Auto-generates queries and actions for a Prisma entity. Each operation in
`operations` can be enabled with defaults (an empty object), made public
via `isPublic`, or replaced by a custom implementation via `overrideFn`.

See [Automatic CRUD](https://wasp.sh/docs/data-model/crud).

## Parameters

### name

`string`

Unique name for the generated CRUD.

### entity

`string`

Name of the Prisma entity to generate operations for.

### operations

[`CrudOperations`](../interfaces/CrudOperations.md)

Which operations to generate and how to configure each.

<!-- Used as a shared snippet in multiple different files with @include helper. -->

Use [reference imports](https://wasp.sh/docs/general/spec#reference-imports)
when passing values from your app into the Wasp spec. If a direct import is not
practical, use [ref](ref.md) as a fallback.

## Returns

[`Crud`](../interfaces/Crud.md)

## Example

```ts
import { crud } from '@wasp.sh/spec'
import { createTaskOverride } from './src/actions' with { type: 'ref' }

crud('tasks', 'Task', {
  getAll: { isPublic: true },
  get: {},
  create: { overrideFn: createTaskOverride },
  update: {},
})
```
