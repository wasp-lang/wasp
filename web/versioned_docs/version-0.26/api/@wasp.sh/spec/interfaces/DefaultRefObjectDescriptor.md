# Interface: DefaultRefObjectDescriptor

Default import reference, equivalent to
`import SomeValue from "./src/someModule" with { type: "ref" }`.

## Properties

### from

> **from**: `string`

Module path, relative to the `*.wasp.ts` file using it.

***

### importDefault

> **importDefault**: `string`

Local name for the default import.
