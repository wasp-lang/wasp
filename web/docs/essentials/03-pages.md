---
title: Pages & Routes
---

import useBaseUrl from '@docusaurus/useBaseUrl';
import { ShowForTs } from '@site/src/components/TsJsHelpers';


In the default `main.wasp` file created by `wasp new`, there is a __page__ and a __route__ declaration:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
route RootRoute { path: "/", to: MainPage }

page MainPage {
  // We specify that the React implementation of the page is the default export
  // of `src/client/MainPage.jsx`. This statement uses standard JS import syntax.
  // Use `@client` to reference files inside the `src/client` folder.
  component: import Main from "@client/MainPage.jsx"
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
route RootRoute { path: "/", to: MainPage }

page MainPage {
  // We specify that the React implementation of the page is the default export
  // of `src/client/MainPage.tsx`. This statement uses standard JS import syntax.
  // Use `@client` to reference files inside the `src/client` folder.
  component: import Main from "@client/MainPage.tsx"
}
```

</TabItem>
</Tabs>

Together, these declarations tell Wasp that when a user navigates to `/`, it should render the default export from `src/client/MainPage`.

## The MainPage Component

Let's take a look at the React component referenced by the page declaration:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="src/client/MainPage.jsx"
import waspLogo from './waspLogo.png'
import './Main.css'

const MainPage = () => {
  // ...
}
export default MainPage
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/client/MainPage.tsx"
import waspLogo from './waspLogo.png'
import './Main.css'

const MainPage = () => {
  // ...
}
export default MainPage
```

</TabItem>
</Tabs>

Since Wasp uses React for the frontend, this is a normal functional React component. It also uses the CSS and logo image that are located next to it in the `src/client` folder.

That is all the code you need! Wasp takes care of everything else necessary to define, build, and run the web app.

:::tip
`wasp start` automatically picks up the changes you make and restarts the app, so keep it running in the background.
:::

## Adding a Second Page

To add more pages, you can create another set of __page__ and __route__ declarations. You can even add parameters to the URL path, using the same syntax as [React Router](https://reactrouter.com/web/). Let's test this out by adding a new page:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
route HelloRoute { path: "/hello/:name", to: HelloPage }

page HelloPage {
  component: import Hello from "@client/HelloPage.jsx"
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
route HelloRoute { path: "/hello/:name", to: HelloPage }

page HelloPage {
  component: import Hello from "@client/HelloPage.tsx"
}
```

</TabItem>
</Tabs>

When a user visits `/hello/their-name`, Wasp will render the component exported from `src/client/HelloPage` and pass the URL parameter the same way as in React Router:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="src/client/HelloPage.jsx"
const HelloPage = (props) => {
  return (
    <div>Hello {props.match.params.name}!</div>
  )
}

export default HelloPage
```

</TabItem>

<!-- TODO: TypeScript version -->

</Tabs>

Now you can visit `/hello/your-name` and see "Hello your-name!"

## Cleaning Up

Let's prepare for building the Todo app by cleaning up the project and removing files and code we won't need. Start by deleting `Main.css`, `waspLogo.png`, and `HelloPage.jsx` that we just created in the `src/client/` folder.

Since we deleted `HelloPage.jsx`, we also need to remember to remove the `route` and `page` declarations we wrote for it. Your Wasp file should now look like this:

```wasp title="main.wasp"
app TodoApp {
  wasp: {
    version: "^0.11.0"
  },
  title: "Todo app"
}

route RootRoute { path: "/", to: MainPage }

page MainPage {
  component: import Main from "@client/MainPage.jsx"
}
````

Next, we'll remove most of the code from the `MainPage` component:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="src/client/MainPage.jsx"
const MainPage = () => {
  return <div>Hello world!</div>
}

export default MainPage
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/client/MainPage.tsx"
const MainPage = () => {
  return <div>Hello world!</div>
}

export default MainPage
```

</TabItem>
</Tabs>

At this point, the main page should look like this:

<img alt="Todo App - Hello World"
     src={useBaseUrl('img/todo-app-hello-world.png')}
     style={{ border: "1px solid black" }}
/>

In the next section, we'll start implementing some features of the Todo app!
