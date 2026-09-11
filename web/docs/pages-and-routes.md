---
title: Pages and routes
---

import { CardLink } from '@site/src/components/CardLink'

A **page** is a React component that Wasp renders as a full screen of your app. A **route** connects a URL path to a page. You declare both in your `main.wasp.ts` file and Wasp generates the client-side router for you, so you never have to set up [React Router](https://reactrouter.com) by hand.

## Declaring a page and a route

To show a page at a URL, call `route` with a unique name, the URL path, and the page to render. Wrap your React component with `page` to turn it into a page:

```ts title="main.wasp.ts"
import { app, page, route } from "@wasp.sh/spec"
import { MainPage } from "./src/MainPage" with { type: "ref" }
import { AboutPage } from "./src/AboutPage" with { type: "ref" }

export default app({
  // ...
  spec: [
    route("RootRoute", "/", page(MainPage)),
    route("AboutRoute", "/about", page(AboutPage)),
  ],
})
```

The component is a regular React component. It doesn't receive any special props and doesn't need to be exported in a particular way, as long as you import it into the Wasp Spec with a [reference import](./general/spec.md#reference-imports):

```tsx title="src/AboutPage.tsx" auto-js
export function AboutPage() {
  return (
    <main>
      <h1>About us</h1>
      <p>We build things with Wasp.</p>
    </main>
  )
}
```

With this in place, visiting `/about` renders `AboutPage`. Every route needs a unique name (`"AboutRoute"` above). You use that name later to build links to the route in a type-safe way, as shown in [Navigating between pages](#navigating-between-pages).

Wasp collects every route in the `spec` array and turns them into a single React Router configuration. If you have many pages, you can [split the routes across several `*.wasp.ts` files](./general/spec.md#splitting-your-spec-into-multiple-files).

## Reading values from the URL {#dynamic-segments}

Most apps have pages whose content depends on the URL, like `/photo/42`. Instead of declaring a route for every possible photo, put a **dynamic segment** in the path and read its value in the page.

### Parameter segments

Use `:paramName` in a route path to match any value in that segment. Access the matched value in your page component with the `useParams` hook from `react-router`:

```ts title="main.wasp.ts"
import { app, page, route } from "@wasp.sh/spec"
import { PhotoPage } from "./src/PhotoPage" with { type: "ref" }

export default app({
  // ...
  spec: [
    route("PhotoRoute", "/photo/:photoId", page(PhotoPage)),
  ],
})
```

```tsx title="src/PhotoPage.tsx" auto-js
import { useParams } from "react-router"

export function PhotoPage() {
  const { photoId } = useParams<"photoId">()
  return <div>Viewing photo {photoId}</div>
}
```

Read more in the [React Router docs on dynamic segments](https://reactrouter.com/8.0.1/start/data/routing#dynamic-segments).

### Optional segments

Append `?` to a path segment to make it optional. The route matches whether or not the segment is present:

```ts title="main.wasp.ts"
import { app, page, route } from "@wasp.sh/spec"
import { PhotoPage } from "./src/PhotoPage" with { type: "ref" }

export default app({
  // ...
  spec: [
    route("PhotoRoute", "/photo/:photoId/edit?", page(PhotoPage)),
  ],
})
```

```tsx title="src/PhotoPage.tsx" auto-js
import { useParams, useLocation } from "react-router"

export function PhotoPage() {
  const { photoId } = useParams<"photoId">()
  const { pathname } = useLocation()
  const isEditing = pathname.endsWith("/edit")
  return <div>{isEditing ? "Editing" : "Viewing"} photo {photoId}</div>
}
```

Read more in the [React Router docs on optional segments](https://reactrouter.com/8.0.1/start/data/routing#optional-segments).

### Splats

Use `/*` at the end of a route path to match any remaining path segments. Access the matched portion with the `'*'` param:

```ts title="main.wasp.ts"
import { app, page, route } from "@wasp.sh/spec"
import { FilesPage } from "./src/FilesPage" with { type: "ref" }

export default app({
  // ...
  spec: [
    route("FilesRoute", "/files/*", page(FilesPage)),
  ],
})
```

```tsx title="src/FilesPage.tsx" auto-js
import { useParams } from "react-router"

export function FilesPage() {
  const { "*": filePath } = useParams()
  // Visiting /files/docs/report.txt → filePath = "docs/report.txt"
  return <div>File: {filePath}</div>
}
```

Read more in the [React Router docs on splats](https://reactrouter.com/8.0.1/start/data/routing#splats).

### Query strings and hashes

Query strings (`?sortBy=date`) and hashes (`#comments`) are not part of the route path, so any route can receive them. Read them with the `useSearchParams` and `useLocation` hooks from `react-router`:

```tsx title="src/TasksPage.tsx" auto-js
import { useSearchParams } from "react-router"

export function TasksPage() {
  const [searchParams] = useSearchParams()
  const sortBy = searchParams.get("sortBy") ?? "name"
  return <div>Tasks sorted by {sortBy}</div>
}
```

## Navigating between pages

To link from one page to another, use the `Link` component from `wasp/client/router`. It works like React Router's `Link`, but the `to` prop is checked against the routes in your `main.wasp.ts` file, and it asks for the right `params` when the path has dynamic segments:

```tsx title="src/PhotoList.tsx" auto-js
import { Link } from "wasp/client/router"

export function PhotoList({ photoIds }: { photoIds: string[] }) {
  return (
    <ul>
      {photoIds.map((photoId) => (
        <li key={photoId}>
          <Link to="/photo/:photoId" params={{ photoId }}>
            Photo {photoId}
          </Link>
        </li>
      ))}
    </ul>
  )
}
```

When you need to navigate from code instead of from a link, for example after a form submits, build the URL with the `routes` object and pass it to React Router's `useNavigate` hook:

```tsx title="src/NewPhotoForm.tsx" auto-js
import { useNavigate } from "react-router"
import { routes } from "wasp/client/router"

export function NewPhotoForm() {
  const navigate = useNavigate()

  async function handleSubmit() {
    const photoId = await uploadPhoto() // your upload logic
    navigate(routes.PhotoRoute.build({ params: { photoId } }))
  }

  // ...
}
```

`routes` has one entry per route name from your spec. See [Type-safe links](./advanced/links.md) for `NavLink`, search params, hashes, and the full API.

## Restricting a page to logged-in users

If your app uses [authentication](./auth/overview.md), you can mark a page with `authRequired: true`. Wasp then shows the page only to logged-in users and redirects everyone else to the path in `auth.onAuthFailedRedirectTo`:

```ts title="main.wasp.ts"
import { app, page, route } from "@wasp.sh/spec"
import { DashboardPage } from "./src/DashboardPage" with { type: "ref" }

export default app({
  // ...
  auth: {
    // ...
    onAuthFailedRedirectTo: "/login",
  },
  spec: [
    route("DashboardRoute", "/dashboard", page(DashboardPage, { authRequired: true })),
  ],
})
```

A page with `authRequired` receives the logged-in user as the `user` prop:

```tsx title="src/DashboardPage.tsx" auto-js
import type { AuthUser } from "wasp/auth"

export function DashboardPage({ user }: { user: AuthUser }) {
  return <h1>Hello, user {user.id}</h1>
}
```

While Wasp checks whether the user is logged in, it shows a loading indicator instead of the page. Read more about the `user` object and about reading the current user from pages that are open to everyone in [Accessing the logged-in user](./auth/overview.md#accessing-the-logged-in-user).

## Sharing a layout between pages

By default, Wasp renders each page on its own. To give every page a shared header, footer, or set of providers, declare a **root component** in the `client` config and render React Router's `Outlet` where the current page should go:

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"
import { Root } from "./src/Root" with { type: "ref" }

export default app({
  // ...
  client: {
    rootComponent: Root,
  },
})
```

```tsx title="src/Root.tsx" auto-js
import { Outlet } from "react-router"

export function Root() {
  return (
    <div>
      <header>My App</header>
      <Outlet />
      <footer>Made with Wasp</footer>
    </div>
  )
}
```

See [Root Component](./project/client-config.md#root-component) for more details and examples.

## Setting the page title and metadata

The `title` and `head` fields of your `app` config apply to every page. To change the document title or add `<meta>` tags for a single page, render `<title>` and `<meta>` elements inside the page component. React moves them into the document `<head>` for you:

```tsx title="src/PhotoPage.tsx" auto-js
import { useParams } from "react-router"

export function PhotoPage() {
  const { photoId } = useParams<"photoId">()
  return (
    <>
      <title>{`Photo ${photoId} | My App`}</title>
      <meta name="description" content="A photo from my collection" />
      <div>Viewing photo {photoId}</div>
    </>
  )
}
```

Read more in the [React docs on `<title>`](https://react.dev/reference/react-dom/components/title) and [`<meta>`](https://react.dev/reference/react-dom/components/meta), and in the [SEO & GEO](./advanced/seo.md) page.

## Showing a page for unknown URLs

When a visitor opens a URL that no route matches, Wasp shows a generic error screen. To show your own "not found" page instead, add a route with a path of `/*`. The router picks the most specific matching route, so this route only renders when nothing else matches:

```ts title="main.wasp.ts"
import { app, page, route } from "@wasp.sh/spec"
import { NotFoundPage } from "./src/NotFoundPage" with { type: "ref" }

export default app({
  // ...
  spec: [
    // ... your other routes
    route("NotFoundRoute", "/*", page(NotFoundPage)),
  ],
})
```

## Lazy-loaded routes

By default, Wasp lazy-loads all page routes using React Router's [`lazy`](https://reactrouter.com/how-to/code-splitting) property. This means each page's code is only downloaded when the user navigates to it, resulting in smaller initial bundle sizes. This is especially useful for apps with many routes.

If you need a specific route to be eagerly loaded (included in the main bundle), you can set `lazy: false` on the route spec:

```ts title="main.wasp.ts"
import { app, page, route } from "@wasp.sh/spec"
import { DashboardPage } from "./src/DashboardPage" with { type: "ref" }

// This route's page will be included in the initial bundle
export default app({
  // ...
  spec: [
    route("DashboardRoute", "/dashboard", page(DashboardPage), { lazy: false }),
  ],
})
```

:::note
Most apps won't need to change this. Disabling lazy loading is useful when you want to avoid the brief loading delay for a page that users navigate to very frequently, at the cost of a larger initial download.
:::

## Prerendered routes

You can prerender specific routes at build time by setting the `prerender` property. This generates static HTML that is served immediately, giving faster load times and better SEO.

```ts title="main.wasp.ts"
import { app, page, route } from "@wasp.sh/spec"
import { LandingPage } from "./src/LandingPage" with { type: "ref" }

export default app({
  // ...
  spec: [
    route("LandingRoute", "/", page(LandingPage), { prerender: true }),
  ],
})
```

See the [Prerendering](./advanced/prerendering.md) page for the full documentation.

## API Reference

### `page` and `route` specifications

<CardLink
  to="./api/@wasp.sh/spec/functions/page"
  kind="api"
  title="page"
  description="All the options for declaring a page in the Wasp spec."
/>

<CardLink
  to="./api/@wasp.sh/spec/functions/route"
  kind="api"
  title="route"
  description="All the options for declaring a route in the Wasp spec."
/>

### JavaScript API

Wasp exposes the `Link` and `NavLink` components and the `routes` object from `wasp/client/router`. They are documented in the [Type-safe links API reference](./advanced/links.md#api-reference).

Inside page components, you can use every hook and component from `react-router`, such as `useParams`, `useSearchParams`, `useLocation`, `useNavigate`, and `Outlet`. See the [React Router API reference](https://reactrouter.com/8.0.1/api/hooks/useParams).
