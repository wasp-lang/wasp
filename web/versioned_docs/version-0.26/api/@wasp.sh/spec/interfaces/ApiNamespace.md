# Interface: ApiNamespace

Shared middleware applied to every [Api](Api.md) mounted under a path
prefix.

Create one with the [apiNamespace](../functions/apiNamespace.md) constructor.

## Extends

- `BaseSpecElement`\<`"apiNamespace"`\>

## Properties

### kind

> **kind**: `"apiNamespace"`

The internal Wasp type of a [SpecElement](../type-aliases/SpecElement.md). Used by the compiler.
You should not set this field directly, instead use the dedicated constructors.

#### Inherited from

`BaseSpecElement.kind`

***

### middlewareConfigFn

> **middlewareConfigFn**: [`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

Reference to an Express middleware config function for this namespace.

***

### path

> **path**: `string`

Path prefix the namespace applies to (e.g. `"/webhooks"`).
