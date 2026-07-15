# Function: route()

> **route**(`name`, `path`, `page`, `config?`): [`Route`](../interfaces/Route.md)

Creates a [Route](../interfaces/Route.md) definition.

Maps a URL path to a [Page](../interfaces/Page.md). Paths support React Router patterns
such as dynamic segments (`/tasks/:id`), optional segments
(`/photo/:id/edit?`), and splats (`/files/*`).

Use `config.prerender` to render the route to static HTML at build time
(see [Prerendering](https://wasp.sh/docs/advanced/prerendering)) and
`config.lazy` to opt out of lazy-loading the page's bundle.

## Parameters

### name

`string`

Unique name for the route.

### path

`string`

URL path the route matches.

### page

[`Page`](../interfaces/Page.md)

The result of `page()`.

### config?

Optional route settings (`lazy`, `prerender`).

#### lazy?

`boolean`

Lazy-load the page's component.

Set to `false` to include the page in the initial client bundle, which
avoids the brief loading delay on first navigation at the cost of a larger
initial download.

**Default**

```ts
true
```

#### prerender?

`boolean`

If `true`, Wasp renders this page to static HTML at build time. Useful
for SEO and AI crawlers.

Only works on fully static paths (no `:paramName`, `*`, or `?` segments)
and cannot be combined with [Page.authRequired](../interfaces/Page.md#authrequired). See
[Prerendering](https://wasp.sh/docs/advanced/prerendering).

**Default**

```ts
false
```

**Example**

```ts
import { app, page, route } from "@wasp.sh/spec"
import { SomePage } from "./src/SomePage" with { type: "ref" }

export default app({
  // ...
  spec: [
    route("NameRoute", "/some-path", page(SomePage), {
      prerender: true,
    }),
  ],
})
```

## Returns

[`Route`](../interfaces/Route.md)

## Example

```ts
import { page, route } from '@wasp.sh/spec'
import MainPage from './src/MainPage' with { type: 'ref' }

route('MainRoute', '/', page(MainPage))
```
