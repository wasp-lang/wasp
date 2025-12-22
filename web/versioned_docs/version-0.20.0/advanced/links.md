---
title: Type-Safe Links
---

import { Required } from '@site/src/components/Tag'

If you are using Typescript, you can use Wasp's custom `Link` component to create type-safe links to other pages on your site.

## Using the `Link` Component

After you defined a route:

```wasp title="main.wasp"
route TaskRoute { path: "/task/:id", to: TaskPage }
page TaskPage { ... }
```

You can get the benefits of type-safe links by using the `Link` component from `wasp/client/router`:

```jsx title="TaskList.tsx"
import { Link } from 'wasp/client/router'

export const TaskList = () => {
  // ...

  return (
    <div>
      {tasks.map((task) => (
        <Link
          key={task.id}
          to="/task/:id"
          {/* 👆 You must provide a valid path here */}
          params={{ id: task.id }}>
          {/* 👆 All the params must be correctly passed in */}
          {task.description}
        </Link>
      ))}
    </div>
  )
}
```

### Catch-all Routes

If a route path ends with a `/*` pattern (also known as [splat](https://reactrouter.com/en/main/route/route#splats)), you can use the `Link` component like this:

```wasp title="main.wasp"
route CatchAllRoute { path: "/pages/*", to: CatchAllPage }
page CatchAllPage { ... }
```

```jsx title="TaskList.tsx"
<Link to="/pages/*" params={{ '*': 'about' }}>
  About
</Link>
```

This will result in a link like this: `/pages/about`.

### Optional Static Segments

If a route contains optional static segments, you'll need to specify one of the possible paths:

```wasp title="main.wasp"
route OptionalRoute { path: "/task/:id/details?", to: OptionalPage }
page OptionalPage { ... }
```

```jsx title="TaskList.tsx"
/* You can include ... */
<Link to="/task/:id/details" params={{ id: 1 }}>
  Task 1
</Link>

/* ... or exclude the optional segment */
<Link to="/task/:id" params={{ id: 1 }}>
  Task 1
</Link>
```

### Using Search Query & Hash

You can also pass `search` and `hash` props to the `Link` component:

```tsx title="TaskList.tsx"
<Link
  to="/task/:id"
  params={{ id: task.id }}
  search={{ sortBy: 'date' }}
  hash="comments"
>
  {task.description}
</Link>
```

This will result in a link like this: `/task/1?sortBy=date#comments`. Check out the [API Reference](#link-component) for more details.

## The `routes` Object

You can also get all the pages in your app with the `routes` object:

```jsx title="TaskList.tsx"
import { routes } from 'wasp/client/router'

const linkToTask = routes.TaskRoute.build({ params: { id: 1 } })
```

This will result in a link like this: `/task/1`.

### Optional Static Segments

If a route contains optional static segments, you'll need to specify one of the possible paths:

```wasp title="main.wasp"
route OptionalRoute { path: "/task/:id/details?", to: OptionalPage }
page OptionalPage { ... }
```

```tsx title="TaskList.tsx"
const linkToOptional = routes.OptionalRoute.build({
  path: '/task/:id/details', // or '/task/:id'
  params: { id: 1 },
})
```

You can also pass `search` and `hash` props to the `build` function. Check out the [API Reference](#routes-object) for more details.

## API Reference

### `Link` Component

The `Link` component accepts the following props:

- `to` <Required />

  - A valid Wasp Route path from your `main.wasp` file.

    In the case of optional static segments, you must provide one of the possible paths which include or exclude the optional segment. For example, if the path is `/task/:id/details?`, you must provide either `/task/:id/details` or `/task/:id`.

- `params: { [name: string]: string | number }` <Required /> (if the path contains params)

  - An object with keys and values for each param in the path.
  - For example, if the path is `/task/:id`, then the `params` prop must be `{ id: 1 }`. Wasp supports required and optional params.

- `search: string[][] | Record<string, string> | string | URLSearchParams`

  - Any valid input for `URLSearchParams` constructor.
  - For example, the object `{ sortBy: 'date' }` becomes `?sortBy=date`.

- `hash: string`

- all other props that the `react-router-dom`'s [Link](https://reactrouter.com/en/6.26.1/components/link) component accepts

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
        path: '/task/:id/details' | '/task/:id',
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
        params: { '*': ParamValue },
        search?: string[][] | Record<string, string> | string | URLSearchParams
        hash?: string
      }
    ) => // ...
  },
}
```

The `params` object is required if the route contains params. The `search` and `hash` parameters are optional.

You can use the `routes` object like this:

```tsx
import { routes } from 'wasp/client/router'

const linkToRoot = routes.RootRoute.build()
const linkToTask = routes.DetailRoute.build({ params: { id: 1 } })
const linkToOptional = routes.DetailRoute.build({
  path: '/task/:id/details',
  params: { id: 1 },
})
const linkToCatchAll = routes.CatchAllRoute.build({
  params: { '*': 'about' },
})
```
