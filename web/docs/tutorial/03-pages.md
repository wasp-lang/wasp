---
title: "3. Pages and routes: Wasp tutorial"
sidebar_label: "3. Pages and routes"
description: Step 3 of the Wasp tutorial explains the page and route specs in main.wasp.ts, adds a second page with a URL parameter and cleans up the starter project.
---

import useBaseUrl from '@docusaurus/useBaseUrl';
import { ShowForTs } from '@site/src/components/TsJsHelpers';
import WaspStartNote from '../_WaspStartNote.md'
import TypescriptServerNote from '../_TypescriptServerNote.md'
import { TutorialAction } from './TutorialAction';

# 3. Pages and routes

In the default `main.wasp.ts` file created by `wasp new`, there is a **page** and a **route** spec:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```ts title="main.wasp.ts"
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
    ```ts title="main.wasp.ts"
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

To add more pages, you can add another route to your spec. You can even add parameters to the URL path, using [dynamic segments](../features/pages-and-routes#dynamic-segments). Let's test this out by adding a new page:

```ts title="main.wasp.ts"
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
  Since you are using Typescript, you can benefit from using Wasp's type-safe `Link` component and the `routes` object. Check out [Navigating between pages](../features/pages-and-routes#link-component) for more details.
  :::
</ShowForTs>

## Cleaning Up

Now that you've seen how Wasp deals with Routes and Pages, it's finally time to build the Todo app.

Start by cleaning up the starter project and removing unnecessary code and files.

<TutorialAction id="prepare-project" action="APPLY_PATCH">

First, replace the starter styles in `src/Main.css` with the styles for the Todo app. You don't need to understand them to follow the tutorial. They only make the app look nice:

```css title="src/Main.css"
:root {
  --yellow: #f5c842;
  --yellow-dark: #d4a930;
  --black: #111;
  --gray: #555;
  --cream: #f7f5f0;
  --white: #fafafa;
  --font-mono:
    ui-monospace, "SF Mono", Menlo, Consolas, "Liberation Mono", monospace;
  --border: 2px solid var(--black);
}

* {
  box-sizing: border-box;
}

input {
  border-style: solid;
}

body {
  margin: 0;
  background: var(--cream);
  color: var(--black);
  font-family: var(--font-mono);
  line-height: 1.5;
}

header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 0.75rem 1.5rem;
  border-bottom: var(--border);
  background: var(--white);
}

main {
  display: flex;
  flex-direction: column;
  gap: 1rem;
  width: min(32rem, 100% - 2rem);
  margin: 3rem auto;
}

h1 {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  margin: 0;
  font-size: 1.25rem;
}

h1 img {
  height: 2.5rem;
}

h2 {
  margin: 0;
  font-size: 1.5rem;
  text-transform: uppercase;
}

a {
  color: var(--black);
}

:focus-visible {
  outline: 2px solid var(--black);
  outline-offset: 2px;
}

input[type="checkbox"] {
  width: 1.25rem;
  height: 1.25rem;
  margin: 0;
  accent-color: var(--yellow);
}

button,
input[type="submit"] {
  padding: 0.75rem 1.25rem;
  border: var(--border);
  background: var(--yellow);
  color: var(--black);
  font: inherit;
  font-weight: 700;
  cursor: pointer;
}

button:hover,
input[type="submit"]:hover {
  background: var(--yellow-dark);
}

header button {
  padding: 0.5rem 1rem;
}

.card,
.tasks {
  border: var(--border);
  background: var(--white);
  box-shadow: 6px 6px 0 var(--yellow);
}

.card {
  padding: 1rem 2rem 2rem;
}

.new-task-form {
  display: flex;
  gap: 0.5rem;
}

.new-task-form input[type="text"] {
  flex: 1;
  min-width: 0;
  padding: 0.75rem 1rem;
  border: var(--border);
  background: var(--white);
  font: inherit;
}

.tasks {
  margin: 0;
  padding: 0;
  list-style: none;
}

.tasks li {
  display: flex;
  align-items: center;
  gap: 0.75rem;
  padding: 0.75rem 1rem;
}

.tasks li + li {
  border-top: var(--border);
}

.tasks label {
  flex: 1;
  cursor: pointer;
}

.tasks li:has(input:checked) label {
  color: var(--gray);
  text-decoration: line-through;
}
```

Then, replace the `MainPage` component with the skeleton of the Todo app, a header with the app title and an empty main area:

```tsx title="src/MainPage.tsx" auto-js
import Logo from "./assets/wasp-logo-rounded.svg";
import "./Main.css";

export const MainPage = () => {
  return (
    <>
      <header>
        <h1>
          <img src={Logo} alt="Wasp logo" />
          Todo App
        </h1>
      </header>
      <main>Hello world!</main>
    </>
  );
};
```

At this point, the main page should look like this:

<img alt="Todo App - Hello World" src={useBaseUrl('img/todo-app-hello-world.webp')} className="tutorial-image" />

You can now delete `src/HelloPage.{jsx,tsx}` (we won't need this page for the rest of the tutorial).

Since `src/HelloPage.{jsx,tsx}` no longer exists, remove its route from the `main.wasp.ts` file.

Your Wasp file should now look like this:

```ts title="main.wasp.ts"
import { app, page, route } from "@wasp.sh/spec"
import { MainPage } from "./src/MainPage" with { type: "ref" }

export default app({
  name: "TodoApp",
  wasp: { version: "{latestWaspVersion}" },
  title: "TodoApp",
  head: ["<link rel='icon' href='/favicon.ico' />"],
  spec: [
    route("RootRoute", "/", page(MainPage)),
  ],
})
```
</TutorialAction>

Excellent work!

You now have a basic understanding of Wasp and are ready to start building your TodoApp.
We'll implement the app's core features in the following sections.
