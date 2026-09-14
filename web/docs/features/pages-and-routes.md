---
title: Pages and routes
---

import { CardLink } from '@site/src/components/CardLink'
import { Required } from '@site/src/components/Tag'

A **page** is a React component that Wasp renders as a full screen of your app. A **route** connects a URL path to a page. You declare both in your `main.wasp.ts` file and Wasp generates all the wiring for you. Internally, we use the industry-standard [React Router](https://reactrouter.com/8.0.1/home) to handle routing.

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

The component is a regular React component. It doesn't receive any special props and doesn't need to be exported in a particular way:

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

With this example, visiting `/about` would show your `AboutPage` component. Every route needs a unique name (in this case, `"AboutRoute"`), which you can use to build type-safe links between pages, as shown in [Navigating between pages](#navigating-between-pages).

Wasp collects every route in the `spec` array and turns them into a single React Router configuration. If you have many pages, you can [split the routes across several `*.wasp.ts` files](./spec.md#splitting-your-spec-into-multiple-files).

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

To link from one page to another, use the `Link` component from `wasp/client/router`. It behaves the same as [React Router's `Link`](https://reactrouter.com/8.0.1/api/components/Link), but we add types for route paths and parameters. If you give it a route path that doesn't exist or the wrong parameters, you'll get a type error.

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

When you need to navigate from your own code instead of as a link (for example after a form submits), build the URL with the `routes` object. `routes` has one entry per route name from your spec, and you can pass it to React Router's `useNavigate` hook:

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

### Reacting to navigation state with `NavLink`

Use `NavLink` when the current page should be highlighted, or when you want to show a spinner during a pending transition. It takes the same props as `Link`, but `className`, `style`, and `children` can be render-prop functions that receive `{ isActive, isPending, isTransitioning }`.

```tsx title="src/Navigation.tsx" auto-js
import { NavLink } from "wasp/client/router"

export function Navigation() {
  return (
    <nav>
      <NavLink
        to="/tasks"
        className={({ isActive }) =>
          isActive ? "font-bold text-blue-600" : "text-gray-600"
        }
      >
        Tasks
      </NavLink>
    </nav>
  )
}
```

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

See [Root Component](../advanced/client-customization/client-config.md#root-component) for more details and examples.

## Setting the page title and metadata

The `title` and `head` fields of your `app` config apply to every page.

To add metadata for a single page, render `<meta>` elements inside the page component. React moves them into the document `<head>` for you:

```tsx title="src/PhotoPage.tsx" auto-js
import { useParams } from "react-router"

export function PhotoPage() {
  const { photoId } = useParams<"photoId">()
  return (
    <>
      <meta name="description" content="A photo from my collection" />
      <div>Viewing photo {photoId}</div>
    </>
  )
}
```

Read more in the [React docs on `<meta>`](https://react.dev/reference/react-dom/components/meta), and in the [SEO & GEO](../advanced/seo.md) page.

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

By default, Wasp splits and lazy-loads all pages. This means that, for example, while the user is in the `/about` page, all the other pages' code is not loaded. And, when the user navigates to another page, that bundle of code is downloaded on-demand. This reduces the amount of data your users need to download and execute, and thus provides a faster initial load experience, similar to classic HTML sites. This is especially useful for apps with many routes.

Most apps won't need to change this. But if you need a specific route to always be loaded, and have instant rendering, you can set the `lazy: false` option on the `route` spec:

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

:::caution
Disabling lazy loading means that this page's code will always be downloaded by the user's browser ahead of time. This will increase the initial load time of your app, especially if the page has a lot of code.
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

See the [Prerendering](../advanced/prerendering.md) page for the full documentation.

## API Reference

### `page` and `route` specifications

<CardLink
  to="../api/@wasp.sh/spec/functions/page"
  kind="api"
  title="page"
  description="All the options for declaring a page in the Wasp spec."
/>

<CardLink
  to="../api/@wasp.sh/spec/functions/route"
  kind="api"
  title="route"
  description="All the options for declaring a route in the Wasp spec."
/>

### `Link` Component

The `Link` component accepts the following props:

- `to` <Required />

  - A valid Wasp Route path from your `main.wasp.ts` file.

    In the case of optional static segments, you must provide one of the possible paths which include or exclude the optional segment. For example, if the path is `/task/:id/details?`, you must provide either `/task/:id/details` or `/task/:id`.

- `params: { [name: string]: string | number }` <Required /> (if the path contains params)

  - An object with keys and values for each param in the path.
  - For example, if the path is `/task/:id`, then the `params` prop must be `{ id: 1 }`. Wasp supports required and optional params.

- `search: string[][] | Record<string, string> | string | URLSearchParams`

  - Any valid input for `URLSearchParams` constructor.
  - For example, the object `{ sortBy: 'date' }` becomes `?sortBy=date`.

- `hash: string`

- all other props that the `react-router`'s [Link](https://reactrouter.com/8.0.1/api/components/Link) component accepts

### `NavLink` Component

The `NavLink` component accepts the same `to`, `params`, `search`, and `hash` props as the [`Link` component](#link-component), plus:

- all other props that the `react-router`'s [NavLink](https://reactrouter.com/8.0.1/api/components/NavLink) component accepts

  - Notably, `className`, `style`, and `children` accept render-prop functions that receive `{ isActive, isPending, isTransitioning }`, and `end` and `caseSensitive` control how the active match is computed.

### `routes` Object

The `routes` object contains a function for each route in your app.

```ts title="router.tsx"
export const routes = {
  // RootRoute has a path like "/"
  RootRoute: {
    build: (options?: {
      search?: string[][] | Record<string, string> | string | URLSearchParams
      hash?: string
    }) => // ...
  },

  // DetailRoute has a path like "/task/:id/:userId?"
  DetailRoute: {
    build: (
      options: {
        params: { id: ParamValue; userId?: ParamValue; },
        search?: string[][] | Record<string, string> | string | URLSearchParams
        hash?: string
      }
    ) => // ...
  },

  // OptionalRoute has a path like "/task/:id/details?"
  OptionalRoute: {
    build: (
      options: {
        path: "/task/:id/details" | "/task/:id",
        params: { id: ParamValue },
        search?: string[][] | Record<string, string> | string | URLSearchParams
        hash?: string
      }
    ) => // ...
  },

  // CatchAllRoute has a path like "/pages/*"
  CatchAllRoute: {
    build: (
      options: {
        params: { "*": ParamValue },
        search?: string[][] | Record<string, string> | string | URLSearchParams
        hash?: string
      }
    ) => // ...
  },
}
```

The `params` object is required if the route contains params. The `search` and `hash` parameters are optional.

### React Router API

Inside page components, you can use every hook and component from `react-router`, such as [`useParams`](https://reactrouter.com/8.0.1/api/hooks/useParams), [`useSearchParams`](https://reactrouter.com/8.0.1/api/hooks/useSearchParams), [`useLocation`](https://reactrouter.com/8.0.1/api/hooks/useLocation), [`useNavigate`](https://reactrouter.com/8.0.1/api/hooks/useNavigate), and [`Outlet`](https://reactrouter.com/8.0.1/api/components/Outlet).
