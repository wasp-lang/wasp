---
title: 3. Pages & Routes
---

import useBaseUrl from '@docusaurus/useBaseUrl';
import { ShowForTs } from '@site/src/components/TsJsHelpers';
import WaspStartNote from '../\_WaspStartNote.md'
import TypescriptServerNote from '../\_TypescriptServerNote.md'

In the default `main.wasp` file created by `wasp new`, there is a **page** and a **route** declaration:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```wasp title="main.wasp"
    route RootRoute { path: "/", to: MainPage }
    page MainPage {
      // We specify that the React implementation of the page is exported from
      // `src/MainPage.jsx`. This statement uses standard JS import syntax.
      // Use `@src` to reference files inside the `src` folder.
      component: import { MainPage } from "@src/MainPage"
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```wasp title="main.wasp"
    route RootRoute { path: "/", to: MainPage }
    page MainPage {
      // We specify that the React implementation of the page is exported from
      // `src/MainPage.tsx`. This statement uses standard JS import syntax.
      // Use `@src` to reference files inside the `src` folder.
      component: import { MainPage } from "@src/MainPage"
    }
    ```
  </TabItem>
</Tabs>

Together, these declarations tell Wasp that when a user navigates to `/`, it should render the named export from `src/MainPage.{jsx,tsx}`.

## The MainPage Component

Let's take a look at the React component referenced by the page declaration:

```tsx title="src/MainPage.tsx" auto-js
import Logo from './assets/logo.svg';
import './Main.css';

export function MainPage() {
  // ...
}
```

This is a regular functional React component. It also imports some CSS and a logo from the `assets` folder.

That is all the code you need! Wasp takes care of everything else necessary to define, build, and run the web app.

<WaspStartNote />

<ShowForTs>
  <TypescriptServerNote />
</ShowForTs>

## Adding a Second Page

To add more pages, you can create another set of **page** and **route** declarations. You can even add parameters to the URL path, using the same syntax as [React Router](https://reactrouter.com/en/6.26.1). Let's test this out by adding a new page:

```wasp title="main.wasp"
route HelloRoute { path: "/hello/:name", to: HelloPage }
page HelloPage {
  component: import { HelloPage } from "@src/HelloPage"
}
```

When a user visits `/hello/their-name`, Wasp renders the component exported from `src/HelloPage.{jsx,tsx}` and you can use the `useParams` hook from `react-router-dom` to access the `name` parameter:

```tsx title="src/HelloPage.tsx" auto-js
import { useParams } from 'react-router-dom'

export const HelloPage = () => {
  const { name } = useParams<'name'>()
  return <div>Here's {name}!</div>
}
```

Now you can visit `/hello/johnny` and see "Here's johnny!"

<ShowForTs>
  :::tip Type-safe links
  Since you are using Typescript, you can benefit from using Wasp's type-safe `Link` component and the `routes` object. Check out the [type-safe links docs](../advanced/links) for more details.
  :::
</ShowForTs>

## Cleaning Up

Now that you've seen how Wasp deals with Routes and Pages, it's finally time to build the Todo app.

Start by cleaning up the starter project and removing unnecessary code and files.

First, remove most of the code from the `MainPage` component:

```tsx title="src/MainPage.tsx" auto-js
export const MainPage = () => {
  return <div>Hello world!</div>
}
```

At this point, the main page should look like this:

<img alt="Todo App - Hello World" src={useBaseUrl('img/todo-app-hello-world.png')} className="tutorial-image" />

You can now delete redundant files: `src/Main.css`, `src/assets/logo.svg`, and `src/HelloPage.{jsx,tsx}` (we won't need this page for the rest of the tutorial).

Since `src/HelloPage.{jsx,tsx}` no longer exists, remove its `route` and `page` declarations from the `main.wasp` file.

Your Wasp file should now look like this:

```wasp title="main.wasp"
app TodoApp {
  wasp: {
    version: "{latestWaspVersion}"
  },
  title: "TodoApp",
  head: [
    "<link rel='icon' href='/favicon.ico' />",
  ]
}

route RootRoute { path: "/", to: MainPage }
page MainPage {
  component: import { MainPage } from "@src/MainPage"
}
```

Excellent work!

You now have a basic understanding of Wasp and are ready to start building your TodoApp.
We'll implement the app's core features in the following sections.
