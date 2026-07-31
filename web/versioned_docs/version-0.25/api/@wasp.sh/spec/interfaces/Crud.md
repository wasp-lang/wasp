# Interface: Crud

Auto-generated CRUD operations for an entity.

Create one with the [crud](../functions/crud.md) constructor.

See [Automatic CRUD](https://wasp.sh/docs/data-model/crud).

## Extends

- `BaseSpecElement`\<`"crud"`\>

## Properties

### entity

> **entity**: `string`

Entity to generate operations for.

***

### kind

> **kind**: `"crud"`

The internal Wasp type of a [SpecElement](../type-aliases/SpecElement.md). Used by the compiler.
You should not set this field directly, instead use the dedicated constructors.

#### Inherited from

`BaseSpecElement.kind`

***

### name

> **name**: `string`

Unique name for this CRUD.

***

### operations

> **operations**: [`CrudOperations`](CrudOperations.md)

Which operations to generate and how to configure each one.
