# Interface: Query

A server-side read-only operation, callable from the client and the server.

Create one with the [query](../functions/query.md) constructor.

See [Queries](https://wasp.sh/docs/data-model/operations/queries).

## Extends

- `BaseSpecElement`\<`"query"`\>

## Properties

### auth?

> `optional` **auth?**: `boolean`

Whether this Query requires auth. If your app has auth enabled, this
defaults to `true`.

Only available if your app has auth enabled.

***

### entities?

> `optional` **entities?**: `string`[]

A list of entities you wish to use inside your Query.

See [Using Entities in Queries](https://wasp.sh/docs/data-model/operations/queries#using-entities-in-queries).

***

### fn

> **fn**: [`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

Reference to the Query's NodeJS implementation.

See [the
docs](https://wasp.sh/docs/data-model/operations/queries#implementing-queries)
for details on the implementation and its context.

***

### kind

> **kind**: `"query"`

The internal Wasp type of a [SpecElement](../type-aliases/SpecElement.md). Used by the compiler.
You should not set this field directly, instead use the dedicated constructors.

#### Inherited from

`BaseSpecElement.kind`
