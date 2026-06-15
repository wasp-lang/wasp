# Function: apiNamespace()

> **apiNamespace**(`path`, `config`): [`ApiNamespace`](../interfaces/ApiNamespace.md)

Creates an [ApiNamespace](../interfaces/ApiNamespace.md) definition.

Applies a shared middleware function to every [api](api.md) mounted under a
given path prefix. Useful for tweaking middleware (e.g. raw body parsing,
CORS) for a group of related endpoints.

See the
[per-path middleware section](https://wasp.sh/docs/advanced/middleware-config#3-customize-per-path-middleware).

## Parameters

### path

`string`

Path prefix the namespace applies to.

### config

Required `middlewareConfigFn`.

<!-- Used as a shared snippet in multiple different files with @include helper. -->

Use [reference imports](https://wasp.sh/docs/general/spec#reference-imports)
when passing values from your app into the Wasp spec. If a direct import is not
practical, use [ref](ref.md) as a fallback.

#### middlewareConfigFn

[`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

Reference to an Express middleware config function for this namespace.

## Returns

[`ApiNamespace`](../interfaces/ApiNamespace.md)

## Example

```ts
import { apiNamespace } from '@wasp.sh/spec'
import { barMiddleware } from './src/apis' with { type: 'ref' }

apiNamespace('/bar', { middlewareConfigFn: barMiddleware })
```
