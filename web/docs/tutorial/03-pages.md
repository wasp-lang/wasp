---
title: 3. Pages & Routes
---

import useBaseUrl from '@docusaurus/useBaseUrl';
import { ShowForTs } from '@site/src/components/TsJsHelpers';
import WaspStartNote from '../\_WaspStartNote.md'
import TypescriptServerNote from '../\_TypescriptServerNote.md'
import { TutorialAction } from './TutorialAction';

In the default `main.wasp.tsx` file created by `wasp new`, there is a **page** and a **route** spec:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```tsx title="main.wasp.tsx"
    import { app, page, route } from "@wasp.sh/spec"
    import { MainPage } from "./src/MainPage" with { type: "ref" }

    export default app({
      // ...
      spec: [
        // We specify that the React implementation of the page is exported from
        // `src/MainPage.jsx`. Reference imports must point to files inside `src`.
        route("RootRoute", "/", page(MainPage)),
      ],
    })
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```tsx title="main.wasp.tsx"
    import { app, page, route } from "@wasp.sh/spec"
    import { MainPage } from "./src/MainPage" with { type: "ref" }

    export default app({
      // ...
      spec: [
        // We specify that the React implementation of the page is exported from
        // `src/MainPage.tsx`. Reference imports must point to files inside `src`.
        route("RootRoute", "/", page(MainPage)),
      ],
    })
    ```
  </TabItem>
</Tabs>

Together, these specifications tell Wasp that when a user navigates to `/`, it should render the `MainPage` component from `src/MainPage.{jsx,tsx}`.

## The MainPage Component

Let's take a look at the React component referenced by the page spec:

```tsx title="src/MainPage.tsx" auto-js
import Logo from "./assets/wasp-logo-rounded.svg";
import "./Main.css";

export function MainPage() {
  // ...
};
```

This is a regular functional React component. It also imports some CSS and a logo from the `assets` folder.

That is all the code you need! Wasp takes care of everything else necessary to define, build, and run the web app.

<WaspStartNote />

<ShowForTs>
  <TypescriptServerNote />
</ShowForTs>

## Adding a Second Page

To add more pages, you can add another route to your spec. You can even add parameters to the URL path, using [dynamic segments](../advanced/routing#dynamic-segments). Let's test this out by adding a new page:

```tsx title="main.wasp.tsx"
import { app, page, route } from "@wasp.sh/spec"
import { HelloPage } from "./src/HelloPage" with { type: "ref" }

export default app({
  // ...
  spec: [
    route("HelloRoute", "/hello/:name", page(HelloPage)),
  ],
})
```

When a user visits `/hello/their-name`, Wasp renders the component exported from `src/HelloPage.{jsx,tsx}` and you can use the `useParams` hook from `react-router` to access the `name` parameter:

```tsx title="src/HelloPage.tsx" auto-js
import { useParams } from "react-router";

export const HelloPage = () => {
  const { name } = useParams<"name">();
  return <div>Here's {name}!</div>;
};
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

<TutorialAction id="prepare-project" action="APPLY_PATCH">

First, remove most of the code from the `MainPage` component:

```tsx title="src/MainPage.tsx" auto-js
export const MainPage = () => {
  return <div>Hello world!</div>;
};
```

At this point, the main page should look like this:

<img alt="Todo App - Hello World" src={useBaseUrl('img/todo-app-hello-world.png')} className="tutorial-image" />

You can now delete redundant files: `src/Main.css`, `src/assets/wasp-logo-rounded.svg`, and `src/HelloPage.{jsx,tsx}` (we won't need this page for the rest of the tutorial).

Since `src/HelloPage.{jsx,tsx}` no longer exists, remove its route from the `main.wasp.tsx` file.

Your Wasp file should now look like this:

```tsx title="main.wasp.tsx"
import { app, page, route } from "@wasp.sh/spec"
import { MainPage } from "./src/MainPage" with { type: "ref" }

export default app({
  name: "TodoApp",
  wasp: {
    version: "{latestWaspVersion}",
  },
  title: "TodoApp",
  head: [
    <link rel="icon" href="/favicon.ico" />,
  ],
  spec: [
    route("RootRoute", "/", page(MainPage)),
  ],
})
```
</TutorialAction>

Excellent work!

You now have a basic understanding of Wasp and are ready to start building your TodoApp.
We'll implement the app's core features in the following sections.
