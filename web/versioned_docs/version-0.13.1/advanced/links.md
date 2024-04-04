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

You can also pass `search` and `hash` props to the `build` function. Check out the [API Reference](#routes-object) for more details.


## API Reference

### `Link` Component

The `Link` component accepts the following props:
- `to` <Required />

  - A valid Wasp Route path from your `main.wasp` file.

- `params: { [name: string]: string | number }` <Required /> (if the path contains params)

  - An object with keys and values for each param in the path.
  - For example, if the path is `/task/:id`, then the `params` prop must be `{ id: 1 }`. Wasp supports required and optional params.

- `search: string[][] | Record<string, string> | string | URLSearchParams`

  - Any valid input for `URLSearchParams` constructor.
  - For example, the object `{ sortBy: 'date' }` becomes `?sortBy=date`.

- `hash: string`
- all other props that the `react-router-dom`'s [Link](https://v5.reactrouter.com/web/api/Link) component accepts


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

  // DetailRoute has a path like "/task/:id/:something?"
  DetailRoute: {
    build: (
      options: {
        params: { id: ParamValue; something?: ParamValue; },
        search?: string[][] | Record<string, string> | string | URLSearchParams
        hash?: string
      }
    ) => // ...
  }
}
```

The `params` object is required if the route contains params. The `search` and `hash` parameters are optional.

You can use the `routes` object like this:

```tsx
import { routes } from 'wasp/client/router'

const linkToRoot = routes.RootRoute.build()
const linkToTask = routes.DetailRoute.build({ params: { id: 1 } })
```
