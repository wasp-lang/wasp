---
title: Migration from 0.14.X to 0.15.X
---

## What's new in 0.15.0?

Wasp 0.15.0 brings upgrades to some of Wasp's most important dependencies. Let's see what's new.

### Prisma 5

Wasp is now using the latest Prisma 5, which brings a lot of performance improvements and new features.

From the Prisma docs:

> Prisma ORM 5.0.0 introduces a number of changes, including the usage of our new JSON Protocol, which make Prisma Client faster by default.

This means that your Wasp app will be faster and more reliable with the new Prisma 5 version.

### React Router 6

Wasp also upgraded its React Router version from `5.3.4` to `6.26.2`. This means that we are now using the latest React Router version, which brings us up to speed and opens up new possibilities for Wasp e.g. potentially using loaders and actions in the future.

There are some breaking changes in React Router 6, so you will need to update your app to use the new hooks and components.

## How to migrate?

To migrate your Wasp app from 0.14.X to 0.15.X, follow these steps:

### 1. Bump the Wasp version

Update the version field in your Wasp file to `^0.15.0`:

```wasp title="main.wasp"
app MyApp {
  wasp: {
    // highlight-next-line
    version: "^0.15.0"
  },
}
```

### 2. Update the `package.json` file

1. Update the `prisma` version in your `package.json` file to `5.19.1`, and add `"type": "module"` to the top level:

   ```json title="package.json"
   {
     ...
      // highlight-next-line
     "type": "module",
     "dependencies": {
       ....
       // highlight-next-line
       "prisma": "5.19.1"
     }
     ...
   }
   ```

1. If you have `@types/react-router-dom` in your `package.json`, you can remove it as it is no longer needed.

### 3. Use the latest React Router APIs

Update the usage of the old React Router 5 APIs to the new React Router 6 APIs:

1.  If you used the `useHistory()` hook, you should now use the `useNavigate()` hook.

      <Tabs>
      <TabItem value="before" label="Before">

    ```tsx title="src/SomePage.tsx"
    import { useHistory } from 'react-router-dom'

    export function SomePage() {
      const history = useHistory()
      const handleClick = () => {
        // highlight-next-line
        history.push('/new-route')
      }
      return <button onClick={handleClick}>Go to new route</button>
    }
    ```

      </TabItem>
      <TabItem value="after" label="After">

    ```tsx title="src/SomePage.tsx"
    import { useNavigate } from 'react-router-dom'

    export function SomePage() {
      const navigate = useNavigate()
      const handleClick = () => {
        // highlight-next-line
        navigate('/new-route')
      }
      return <button onClick={handleClick}>Go to new route</button>
    }
    ```

      </TabItem>
      </Tabs>

    Check the [React Router 6 docs](https://reactrouter.com/en/main/hooks/use-navigate#optionsreplace) for more information on the `useNavigate()` hook.

1.  If you used the `<Redirect />` component, you should now use the `<Navigate />` component.

    The default behaviour changed from `replace` to `push` in v6, so if you want to keep the old behaviour, you should add the `replace` prop.

      <Tabs>
      <TabItem value="before" label="Before">

    ```tsx title="src/SomePage.tsx"
    import { Redirect } from 'react-router-dom'

    export function SomePage() {
      return (
        // highlight-next-line
        <Redirect to="/new-route" />
      )
    }
    ```

      </TabItem>
      <TabItem value="after" label="After">

    ```tsx title="src/SomePage.tsx"
    import { Navigate } from 'react-router-dom'

    export function SomePage() {
      return (
        // highlight-next-line
        <Navigate to="/new-route" replace />
      )
    }
    ```

      </TabItem>
      </Tabs>

    Check the [React Router 6 docs](https://reactrouter.com/en/main/components/navigate) for more information on the `<Navigate />` component.

1.  If you accessed the route params using `props.match.params`, you should now use the `useParams()` hook.

      <Tabs>
      <TabItem value="before" label="Before">

    ```tsx title="src/SomePage.tsx"
    import { RouteComponentProps } from 'react-router-dom'

    export function SomePage(props: RouteComponentProps) {
      // highlight-next-line
      const { id } = props.match.params
      return (
        <div>
          <h1>Item {id}</h1>
        </div>
      )
    }
    ```

      </TabItem>
      <TabItem value="after" label="After">

    ```tsx title="src/SomePage.tsx"
    import { useParams } from 'react-router-dom'

    export function SomePage() {
      // highlight-next-line
      const { id } = useParams()
      return (
        <div>
          <h1>Item {id}</h1>
        </div>
      )
    }
    ```

      </TabItem>
      </Tabs>

    Check the [React Router 6 docs](https://reactrouter.com/en/main/hooks/use-params) for more information on the `useParams()` hook.

1.  If you used the `<NavLink />` component and its `isActive` prop to set the active link state, you should now set the `className` prop directly.

      <Tabs>
      <TabItem value="before" label="Before">

    ```tsx title="src/SomePage.tsx"
    import { NavLink } from 'react-router-dom'

    export function SomePage() {
      return (
        <NavLink
          to="/new-route"
          // highlight-start
          isActive={(_match, location) => {
            return location.pathname === '/new-route'
          }}
          // highlight-end
          className={(isActive) =>
            cn('text-blue-500', {
              underline: isActive,
            })
          }
        >
          Go to new route
        </NavLink>
      )
    }
    ```

      </TabItem>
      <TabItem value="after" label="After">

    ```tsx title="src/SomePage.tsx"
    import { NavLink, useLocation } from 'react-router-dom'

    export function SomePage() {
      // highlight-next-line
      const location = useLocation()
      return (
        <NavLink
          to="/new-route"
          className={() =>
            cn('text-blue-500', {
              // highlight-next-line
              underline: location.pathname === '/new-route',
            })
          }
        >
          Go to new route
        </NavLink>
      )
    }
    ```

      </TabItem>
      </Tabs>

    Check the [React Router 6 docs](https://reactrouter.com/en/main/components/nav-link#navlink) for more information on the `<NavLink />` component.

### 4. Update your root component

The `client.rootComponent` now requires rendering `<Outlet />` instead the `children` prop.

<Tabs>
<TabItem value="before" label="Before">

```wasp title="main.wasp"
app MyApp {
  title: "My app",
  // ...
  client: {
    rootComponent: import { App } from "@src/App.tsx",
  }
}
```

```tsx title="src/App.tsx"
export function App({ children }: { children: React.ReactNode }) {
  return (
    <div>
      <header>
        <h1>My App</h1>
      </header>
      // highlight-next-line
      {children}
      <footer>
        <p>My App footer</p>
      </footer>
    </div>
  )
}
```

</TabItem>
<TabItem value="after" label="After">

```wasp title="main.wasp"
app MyApp {
  title: "My app",
  // ...
  client: {
    rootComponent: import { App } from "@src/App.tsx",
  }
}
```

```tsx title="src/App.tsx"
import { Outlet } from 'react-router-dom'

export function App() {
  return (
    <div>
      <header>
        <h1>My App</h1>
      </header>
      // highlight-next-line
      <Outlet />
      <footer>
        <p>My App footer</p>
      </footer>
    </div>
  )
}
```

</TabItem>
</Tabs>

That's it!

You should now be able to run your app with the new Wasp 0.15.0.
