# Interface: NamedRefObjectDescriptor

Named import reference, equivalent to
`import { SomeValue } from "./src/someModule" with { type: "ref" }`.

## Properties

### alias?

> `optional` **alias?**: `string`

Optional local alias.

Alias takes precedence over the `import` field when
Wasp Spec dervies some [WaspSpec.SpecElement](../type-aliases/SpecElement.md) name.

***

### from

> **from**: `string`

Module path, relative to the `*.wasp.ts` file using it.

***

### import

> **import**: `string`

Exported name to import.
