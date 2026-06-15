# Function: page()

> **page**(`component`, `config?`): [`Page`](../interfaces/Page.md)

Creates a [Page](../interfaces/Page.md) definition.

A page is a React component rendered by a [route](route.md).

See [Routing](https://wasp.sh/docs/advanced/routing) and the
[Auth overview](https://wasp.sh/docs/auth/overview#protecting-a-page-with-authrequired)
for protecting pages with `authRequired`.

## Parameters

### component

[`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

The React component to render.

<!-- Used as a shared snippet in multiple different files with @include helper. -->

Use [reference imports](https://wasp.sh/docs/general/spec#reference-imports)
when passing values from your app into the Wasp spec. If a direct import is not
practical, use [ref](ref.md) as a fallback.

### config?

Optional page settings such as `authRequired`.

#### authRequired?

`boolean`

If `true`, only authenticated users can access this page. Unauthenticated
visitors are redirected to [Auth.onAuthFailedRedirectTo](../interfaces/Auth.md#onauthfailedredirectto).

Cannot be combined with [Route.prerender](../interfaces/Route.md#prerender).

**Default**

```ts
false
```

## Returns

[`Page`](../interfaces/Page.md)

## Example

```ts
import { page } from '@wasp.sh/spec'
import MainPage from './src/MainPage' with { type: 'ref' }

page(MainPage, { authRequired: true })
```
