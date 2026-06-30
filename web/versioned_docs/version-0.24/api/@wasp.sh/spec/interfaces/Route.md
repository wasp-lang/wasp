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

> `optional` **prerender?**: `boolean`

If `true`, Wasp renders this page to static HTML at build time. Useful
for SEO and AI crawlers.

Only works on fully static paths (no `:paramName`, `*`, or `?` segments)
and cannot be combined with [Page.authRequired](Page.md#authrequired). See
[Prerendering](https://wasp.sh/docs/advanced/prerendering).

#### Default

```ts
false
```

#### Example

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
