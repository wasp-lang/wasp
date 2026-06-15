# Interface: CrudOperationOptions

Configuration for a single CRUD operation.

## Properties

### isPublic?

> `optional` **isPublic?**: `boolean`

If `true`, the operation skips Wasp's auth check and can be called by
anyone.

#### Default

```ts
false
```

***

### overrideFn?

> `optional` **overrideFn?**: [`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

Reference to a custom implementation that replaces Wasp's auto-generated
one. Use this when you need custom business logic for an operation (e.g.
attaching `userId` on `create`).

See the [CRUD guide](https://wasp.sh/docs/data-model/crud#adding-crud-to-the-task-entity-)
for an override example.

The override receives the caller-provided `args` payload and a
Wasp-provided `context` containing the current user and the entity being
operated on.

**Important**
Use an override to validate or filter input before saving it;
the default `create` and `update` implementations pass client-sent data to
Prisma.
