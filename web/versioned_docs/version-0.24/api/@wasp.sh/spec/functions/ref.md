# Function: ref()

> **ref**(`_descriptor`): [`RefObject`](../type-aliases/RefObject.md)

Creates a fallback reference object for a value from your app.

<!-- Used as a shared snippet in multiple different files with @include helper. -->

Use [reference imports](https://wasp.sh/docs/general/spec#reference-imports)
when passing values from your app into the Wasp spec. If a direct import is not
practical, use  ref as a fallback.

Reference imports are preferred because editors can follow and rename real
imports.

The import path must be relative to the `*.wasp.ts` file where it is used
and resolve inside the app's `src/` directory. Absolute
paths are not supported.

## Parameters

### \_descriptor

[`RefObjectDescriptor`](../type-aliases/RefObjectDescriptor.md)

## Returns

[`RefObject`](../type-aliases/RefObject.md)

## Example

```ts
import { page, ref } from "@wasp.sh/spec"

const MainPage = ref({
  importDefault: "MainPage",
  from: "./src/MainPage",
})

export const mainPage = page(MainPage)
```
