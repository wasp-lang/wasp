# Type Alias: ZodSchema

> **ZodSchema** = `object`

Structural type for a runtime Zod schema accepted by Wasp env validation fields.

<!-- Used as a shared snippet in multiple different files with @include helper. -->

Use [reference imports](https://wasp.sh/docs/general/spec#reference-imports)
when passing values from your app into the Wasp spec. If a direct import is not
practical, use [ref](../functions/ref.md) as a fallback.

See [Server.envValidationSchema](../interfaces/Server.md#envvalidationschema) and
[Client.envValidationSchema](../interfaces/Client.md#envvalidationschema) for examples.

To avoid depending on the `zod` package, Wasp structurally recognizes Zod 4
schemas via their documented library-author marker. See
https://zod.dev/library-authors.

## Properties

### \_zod

> `readonly` **\_zod**: `object`

#### def

> `readonly` **def**: `object`
