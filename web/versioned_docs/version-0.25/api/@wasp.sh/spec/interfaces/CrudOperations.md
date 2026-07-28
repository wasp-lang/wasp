# Interface: CrudOperations

Mapping of CRUD operations to their options.

Each key enables the matching operation; an empty object enables it with
Wasp's defaults.

The default implementations map directly to Prisma calls on the entity, and
use the field marked with `@id` in the Prisma schema as the entity ID. Unless
an operation is marked with [CrudOperationOptions.isPublic](CrudOperationOptions.md#ispublic), Wasp
checks that an authenticated user is making the request.

CRUD operations are implemented with Wasp queries and actions. See
[Operations](https://wasp.sh/docs/data-model/operations/overview).

## Properties

### create?

> `optional` **create?**: [`CrudOperationOptions`](CrudOperationOptions.md)

#### Default

`Entity.create({ data: args.data })`

***

### delete?

> `optional` **delete?**: [`CrudOperationOptions`](CrudOperationOptions.md)

#### Default

`Entity.delete({ where: { id: args.id } })`

***

### get?

> `optional` **get?**: [`CrudOperationOptions`](CrudOperationOptions.md)

#### Default

`Entity.findUnique({ where: { id: args.id } })`

***

### getAll?

> `optional` **getAll?**: [`CrudOperationOptions`](CrudOperationOptions.md)

#### Default

`Entity.findMany()`

***

### update?

> `optional` **update?**: [`CrudOperationOptions`](CrudOperationOptions.md)

#### Default

`Entity.update({ where: { id: args.id }, data: args.data })`
