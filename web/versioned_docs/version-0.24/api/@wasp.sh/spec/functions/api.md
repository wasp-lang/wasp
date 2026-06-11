# Function: api()

> **api**(`method`, `path`, `fn`, `config?`): [`Api`](../interfaces/Api.md)

Creates an [Api](../interfaces/Api.md) endpoint definition.

APIs are custom HTTP endpoints handled by a plain Express function. Use
them for webhooks, file uploads, or any HTTP interaction that doesn't fit
the [query](query.md)/[action](action.md) model.

See [Custom HTTP API Endpoints](https://wasp.sh/docs/advanced/apis).

## Parameters

### method

[`HttpMethod`](../type-aliases/HttpMethod.md)

HTTP method to listen on (or `"ALL"` for any).

### path

`string`

Express path the endpoint is mounted at.

### fn

[`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

The API's NodeJS implementation.

<!-- Used as a shared snippet in multiple different files with @include helper. -->

Use [reference imports](https://wasp.sh/docs/general/spec#reference-imports)
when passing values from your app into the Wasp spec. If a direct import is not
practical, use [ref](ref.md) as a fallback.

### config?

Optional settings: `middlewareConfigFn`, `entities`, `auth`.

#### auth?

`boolean`

If `true`, the handler requires the request to come from an
authenticated user and receives `context.user`. Defaults to `true` when
the app has auth enabled, and `false` otherwise. Set to `false` to skip
parsing the JWT from the Authorization header.

#### entities?

`string`[]

Entities the handler operates on. Wasp injects a Prisma delegate for
each one into the handler's `context.entities`.

See [Using Entities in APIs](https://wasp.sh/docs/advanced/apis#using-entities-in-apis).

#### middlewareConfigFn?

[`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

Reference to an Express middleware config function for this endpoint only.

See [Configuring API middleware](https://wasp.sh/docs/advanced/middleware-config#2-customize-api-specific-middleware).

## Returns

[`Api`](../interfaces/Api.md)

## Example

```ts
import { api } from '@wasp.sh/spec'
import { barBaz } from './src/apis' with { type: 'ref' }

api('GET', '/bar/baz', barBaz, { entities: ['Task'], auth: false })
```
