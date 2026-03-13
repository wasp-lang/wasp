---
title: Routing
---

Wasp uses [React Router](https://reactrouter.com) under the hood. Route paths support all the standard patterns described below.

## Dynamic Segments

Use `:paramName` in a route path to match any value in that segment. Access the matched value in your page component with the `useParams` hook from `react-router`.

```wasp title="main.wasp"
route PhotoRoute { path: "/photo/:photoId", to: PhotoPage }
page PhotoPage {
  component: import { PhotoPage } from "@src/PhotoPage"
}
```

```tsx title="src/PhotoPage.tsx" auto-js
import { useParams } from 'react-router'

export function PhotoPage() {
  const { photoId } = useParams<'photoId'>()
  return <div>Viewing photo {photoId}</div>
}
```

Read more in the [React Router docs on dynamic segments](https://reactrouter.com/7.12.0/start/data/routing#dynamic-segments).

## Optional Segments

Append `?` to a path segment to make it optional. The route will match whether or not the segment is present.

```wasp title="main.wasp"
route PhotoRoute { path: "/photo/:photoId/edit?", to: PhotoPage }
page PhotoPage {
  component: import { PhotoPage } from "@src/PhotoPage"
}
```

```tsx title="src/PhotoPage.tsx" auto-js
import { useParams, useLocation } from 'react-router'

export function PhotoPage() {
  const { photoId } = useParams<'photoId'>()
  const { pathname } = useLocation()
  const isEditing = pathname.endsWith('/edit')
  return <div>{isEditing ? 'Editing' : 'Viewing'} photo {photoId}</div>
}
```

Read more in the [React Router docs on optional segments](https://reactrouter.com/7.12.0/start/data/routing#optional-segments).

## Splats

Use `/*` at the end of a route path to match any remaining path segments. Access the matched portion with the `'*'` param.

```wasp title="main.wasp"
route FilesRoute { path: "/files/*", to: FilesPage }
page FilesPage {
  component: import { FilesPage } from "@src/FilesPage"
}
```

```tsx title="src/FilesPage.tsx" auto-js
import { useParams } from 'react-router'

export function FilesPage() {
  const { '*': filePath } = useParams()
  // Visiting /files/docs/report.txt → filePath = "docs/report.txt"
  return <div>File: {filePath}</div>
}
```

Read more in the [React Router docs on splats](https://reactrouter.com/7.12.0/start/data/routing#splats).

## Lazy-Loaded Routes

By default, Wasp lazy-loads all page routes using React Router's [`lazy`](https://reactrouter.com/how-to/code-splitting) property. This means each page's code is only downloaded when the user navigates to it, resulting in smaller initial bundle sizes -- especially useful for apps with many routes.

If you need a specific route to be eagerly loaded (included in the main bundle), you can set `lazy: false` on the route declaration:

```wasp title="main.wasp"
// This route's page will be included in the initial bundle
route DashboardRoute { path: "/dashboard", to: DashboardPage, lazy: false }
```

:::note
Most apps won't need to change this. Disabling lazy loading is useful when you want to avoid the brief loading delay for a page that users navigate to very frequently, at the cost of a larger initial download.
:::
