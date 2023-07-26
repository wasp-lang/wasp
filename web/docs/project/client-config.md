---
title: Client Config
---

You can configure the client using the `client` field inside the `app` declaration,

```wasp
app MyApp {
  title: "My app",
  // ...
  client: {
    rootComponent: import Root from "@client/Root.jsx",
    setupFn: import mySetupFunction from "@client/myClientSetupCode.js"
  }
}
```

`app.client` is a dictionary with the following fields:

#### `rootComponent: ClientImport` (optional)

`rootComponent` defines the root component of your client application. It is
expected to be a React component, and Wasp will use it to wrap your entire app.
It must render its children, which are the actual pages of your application.

You can use it to define a common layout for your application:

```jsx title="src/client/Root.jsx"
export default async function Root({ children }) {
  return (
    <div>
      <header>
        <h1>My App</h1>
      </header>
      {children}
      <footer>
        <p>My App footer</p>
      </footer>
    </div>
  )
}
```

Or, you can use it to set up various providers that your application needs:

```jsx title="src/client/Root.jsx"
import store from './store'
import { Provider } from 'react-redux'

export default async function Root({ children }) {
  return (
    <Provider store={store}>
      {children}
    </Provider>
  )
}
```

As long as you render the children, you can do whatever you want in your root
component. Here's an example of a root component that both sets up a provider and
renders a custom layout:

```jsx title="src/client/Root.jsx"
import store from './store'
import { Provider } from 'react-redux'

export default function Root({ children }) {
  return (
    <Provider store={store}>
      <Layout>
        {children}
      </Layout>
    </Provider>
  )
}

function Layout({ children }) {
  return (
    <div>
      <header>
        <h1>My App</h1>
      </header>
      {children}
      <footer>
        <p>My App footer</p>
      </footer>
    </div>
  )
}
```


#### `setupFn: ClientImport` (optional)

`setupFn` declares a JavaScript function that Wasp executes on the client
before everything else. It is expected to be asynchronous, and
Wasp will await its completion before rendering the page. The function takes no
arguments, and its return value is ignored.

You can use this function to perform any custom setup (e.g., setting up
client-side periodic jobs).

Here's a dummy example of such a function:

```js title="src/client/myClientSetupCode.js"
export default async function mySetupFunction() {
  let count = 1;
  setInterval(
    () => console.log(`You have been online for ${count++} hours.`),
    1000 * 60 * 60,
  )
}
```

##### Overriding default behaviour for Queries
As mentioned, our `useQuery` hook uses _react-query_'s hook of the same name.
Since _react-query_ comes configured with aggressive but sane default options,
you most likely won't have to change those defaults for all Queries (you can
change them for a single Query using the `options` object, as described
[here](#the-usequery-hook)).

Still, if you do need the global defaults, you can do so inside client setup
function. Wasp exposes a `configureQueryClient` hook that lets you configure
_react-query_'s `QueryClient` object:


```js title="src/client/myClientSetupCode.js"
import { configureQueryClient } from '@wasp/queryClient'

export default async function mySetupFunction() {
  // ... some setup
  configureQueryClient({
    defaultOptions: {
      queries: {
        staleTime: Infinity,
      }
    }
  })
  // ... some more setup
}
```

Make sure to pass in an object expected by the `QueryClient`'s constructor, as
explained in
[_react-query_'s docs](https://tanstack.com/query/v4/docs/react/reference/QueryClient).
