---
title: Type-Safe Links
---

import { Required } from '@site/src/components/Tag'

If you are using Typescript, Wasp gives you typesafe building blocks for navigation. You get autocompletion on route paths, compile errors when params are missing, and a single source of truth between your `main.wasp` file and your client code.

## Typesafe navigation with components

For navigating between pages inside JSX, Wasp exposes two components from `wasp/client/router`: `Link` for simple links, and `NavLink` when you need to react to navigation state.

### Simple links with `Link`

Reach for `Link` when you just need to send the user to another page. Given this route:

```wasp title="main.wasp"
route TaskRoute { path: "/task/:id", to: TaskPage }
page TaskPage { ... }
```

You'd use it like this:

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
          {/* 👆 Autocompleted from your wasp routes */}
          params={{ id: task.id }}>
          {/* 👆 Required and typechecked against the path */}
          {task.description}
        </Link>
      ))}
    </div>
  )
}
```

The `to` prop is autocompleted from the routes you defined in `main.wasp`, and `params` is typechecked against the path you picked. Rename a route or change a param, and any broken `Link` lights up at compile time.

### Reacting to navigation state with `NavLink`

Use `NavLink` when the current page should be highlighted, or when you want to show a spinner during a pending transition. It takes the same props as `Link`, but `className`, `style`, and `children` can be render-prop functions that receive `{ isActive, isPending, isTransitioning }`.

```tsx title="Navigation.tsx"
import { NavLink } from 'wasp/client/router'

export const Navigation = () => {
  return (
    <nav>
      <NavLink
        to="/tasks"
        className={({ isActive }) =>
          isActive ? 'font-bold text-blue-600' : 'text-gray-600'
        }
      >
        Tasks
      </NavLink>
    </nav>
  )
}
```

Everything below applies to both `Link` and `NavLink`.

### Catch-all routes

If a route path ends with a `/*` pattern (also known as [splat](https://reactrouter.com/7.12.0/start/declarative/routing#splats)), pass the rest of the path as the `*` param:

```wasp title="main.wasp"
route CatchAllRoute { path: "/pages/*", to: CatchAllPage }
page CatchAllPage { ... }
```

```jsx title="TaskList.tsx"
<Link to="/pages/*" params={{ '*': 'about' }}>
  About
</Link>
```

This renders as `/pages/about`.

### Optional static segments

If a route has an optional static segment, you can choose at the call site whether to include it or not:

```wasp title="main.wasp"
route OptionalRoute { path: "/task/:id/details?", to: OptionalPage }
page OptionalPage { ... }
```

```jsx title="TaskList.tsx"
/* You can include the optional segment ... */
<Link to="/task/:id/details" params={{ id: 1 }}>
  Task 1
</Link>

/* ... or leave it out */
<Link to="/task/:id" params={{ id: 1 }}>
  Task 1
</Link>
```

### Search params and hash

You can also pass `search` and `hash` to attach a query string and fragment:

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

This renders as `/task/1?sortBy=date#comments`. Check out the [API Reference](#link-component) for the full list of accepted props.

## Typesafe navigation outside of components

When you need a URL string instead of a component, for example for `useNavigate`, redirects, `window.location`, or anywhere you are not rendering JSX, use the `routes` object from `wasp/client/router`:

```jsx title="TaskList.tsx"
import { routes } from 'wasp/client/router'

const linkToTask = routes.TaskRoute.build({ params: { id: 1 } })
```

`linkToTask` is the string `/task/1`. Each route from `main.wasp` shows up on `routes` with a `build` function whose options are typed against the route's path, so the same compile-time safety you get from `Link` is also available outside of JSX.

`build` follows the same rules as the [components above](#typesafe-navigation-with-components): catch-all routes take a `*` param, optional static segments pick a concrete `path`, and you can attach a query string and fragment via `search` and `hash`.

```tsx
const linkToTaskComments = routes.OptionalRoute.build({
  path: '/task/:id/details',
  params: { id: 1 },
  search: { sortBy: 'date' },
  hash: 'comments',
})
```

This renders as `/task/1/details?sortBy=date#comments`. Check out the [API Reference](#routes-object) for the full shape.

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

- all other props that the `react-router`'s [Link](https://reactrouter.com/7.12.0/api/components/Link) component accepts

### `NavLink` Component

The `NavLink` component accepts the following props:

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

- all other props that the `react-router`'s [NavLink](https://reactrouter.com/7.12.0/api/components/NavLink) component accepts

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
