# Interface: Route

A URL path mapped to a [Page](Page.md).

Create one with the [route](../functions/route.md) constructor.

See [Routing](https://wasp.sh/docs/advanced/routing) for path patterns
(dynamic segments, optional segments, splats).

## Extends

- `BaseSpecElement`\<`"route"`\>

## Properties

### kind

> **kind**: `"route"`

The internal Wasp type of a [SpecElement](../type-aliases/SpecElement.md). Used by the compiler.
You should not set this field directly, instead use the dedicated constructors.

#### Inherited from

`BaseSpecElement.kind`

***

### lazy?

> `optional` **lazy?**: `boolean`

Lazy-load the page's component.

Set to `false` to include the page in the initial client bundle, which
avoids the brief loading delay on first navigation at the cost of a larger
initial download.

#### Default

```ts
true
```

***

### name

> **name**: `string`

Unique route name.

***

### page

> **page**: [`Page`](Page.md)

Page rendered when this route matches.

***

### path

> **path**: `string`

URL path. Supports React Router patterns like `/tasks/:id`,
`/photo/:id/edit?`, and `/files/*`.

***

### prerender?

> `optional` **prerender?**: `boolean` \| readonly `string`[]

Render this route to static HTML at build time. Useful for SEO and AI
crawlers. The page then hydrates on the client for full interactivity.

Accepts either:
- `true` — prerender this route's own path. The path must be fully static
  (no `:paramName`, `*`, or `?` segments).
- an array of concrete paths to prerender. Use this to prerender specific
  instances of a dynamic route (e.g. `["/blog/intro", "/blog/changelog"]`
  for a `"/blog/:slug"` route). Every listed path must be fully static and
  must match this route's path pattern.

In either case the route's page cannot have [Page.authRequired](Page.md#authrequired) set,
since prerendered content can't depend on the logged-in user. See
[Prerendering](https://wasp.sh/docs/advanced/prerendering).

#### Default

```ts
false
```

#### Example

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
