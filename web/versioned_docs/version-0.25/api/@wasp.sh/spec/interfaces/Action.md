# Interface: Action

A server-side write operation, callable from the client and the server.

Create one with the [action](../functions/action.md) constructor.

See [Actions](https://wasp.sh/docs/data-model/operations/actions).

## Extends

- `BaseSpecElement`\<`"action"`\>

## Properties

### auth?

> `optional` **auth?**: `boolean`

Whether this Action requires auth. If your app has auth enabled, this
defaults to `true`.

Only available if your app has auth enabled.

***

### entities?

> `optional` **entities?**: `string`[]

A list of entities you wish to use inside your Action.

See [Using Entities in Actions](https://wasp.sh/docs/data-model/operations/actions#using-entities-in-actions).

***

### fn

> **fn**: [`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

Reference to the Action's NodeJS implementation.

See [the
docs](https://wasp.sh/docs/data-model/operations/actions#implementing-actions)
for details on the implementation and its context.

***

### kind

> **kind**: `"action"`

The internal Wasp type of a [SpecElement](../type-aliases/SpecElement.md). Used by the compiler.
You should not set this field directly, instead use the dedicated constructors.

#### Inherited from

`BaseSpecElement.kind`
