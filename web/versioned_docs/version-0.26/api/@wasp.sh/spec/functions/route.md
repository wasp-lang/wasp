# Function: route()

> **route**(`name`, `path`, `page`, `config?`): [`Route`](../interfaces/Route.md)

Creates a [Route](../interfaces/Route.md) definition.

Maps a URL path to a [Page](../interfaces/Page.md). Paths support React Router patterns
such as dynamic segments (`/tasks/:id`), optional segments
(`/photo/:id/edit?`), and splats (`/files/*`).

Use `config.prerender` to render the route to static HTML at build time:
`true` prerenders the route's own static path, or pass an array of concrete
paths to prerender specific instances of a dynamic route (see
[Prerendering](https://wasp.sh/docs/advanced/prerendering)). Use
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

`boolean` \| readonly `string`[]

Render this route to static HTML at build time. Useful for SEO and AI
crawlers. The page then hydrates on the client for full interactivity.

Accepts either:
- `true` — prerender this route's own path. The path must be fully static
  (no `:paramName`, `*`, or `?` segments).
- an array of concrete paths to prerender. Use this to prerender specific
  instances of a dynamic route (e.g. `["/blog/intro", "/blog/changelog"]`
  for a `"/blog/:slug"` route). Every listed path must be fully static and
  must match this route's path pattern.

In either case the route's page cannot have [Page.authRequired](../interfaces/Page.md#authrequired) set,
since prerendered content can't depend on the logged-in user. See
[Prerendering](https://wasp.sh/docs/advanced/prerendering).

**Default**

```ts
false
```

**Example**

```ts
import { app, page, route } from "@wasp.sh/spec"
import { LandingPage } from "./src/LandingPage" with { type: "ref" }
import { BlogPostPage } from "./src/BlogPostPage" with { type: "ref" }

export default app({
  // ...
  spec: [
    route("LandingRoute", "/", page(LandingPage), {
      prerender: true,
    }),
    route("BlogPostRoute", "/blog/:slug", page(BlogPostPage), {
      prerender: ["/blog/intro", "/blog/changelog"],
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
