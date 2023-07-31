---
id: 01-creating-new-project
title: "Creating a new project"
---

import useBaseUrl from '@docusaurus/useBaseUrl';
import { ShowForTs } from '@site/src/components/TsJsHelpers';

Run the following command in your terminal to create a new Wasp project:
```shell
wasp new TodoApp
```
Enter the created directory and run:
```shell
cd TodoApp
wasp start
```
You have just run your app in the development mode!

:::note
`wasp start` might take a little bit longer to finish when you run it for the first time due to the initial setup.
:::

You will be seeing a lot of different output from the client, server and database setting themselves up.
Once ready, a new tab should open in your browser at `http://localhost:3000`, with a simple placeholder page:

<img alt="Screenshot of new Wasp app"
     src={useBaseUrl('img/wasp-new-screenshot.png')}
     style={{ border: "1px solid black" }}
     height="400px"
/>

We just set the foundations of our app! We don't have yet the features to show it, but Wasp has already generated for us the full front-end and back-end code of the app. Take a peek at `TodoApp/.wasp/out` if you are curious and want to see what it looks like!

## Taking a closer look at the code

Let's inspect the Wasp project we just created:
```bash
.
├── .gitignore
├── main.wasp             # Your wasp code goes here.
├── src
│   ├── client            # Your client code (JS/CSS/HTML) goes here.
│   │   ├── Main.css
│   │   ├── MainPage.jsx
│   │   ├── vite-env.d.ts
│   │   ├── tsconfig.json
│   │   └── waspLogo.png
│   ├── server            # Your server code (Node JS) goes here.
│   │   └── tsconfig.json
│   ├── shared            # Your shared (runtime independent) code goes here .
│   │   └── tsconfig.json
│   └── .waspignore
└── .wasproot
```
By _your code_, we mean _"the code you write"_ (as opposed to the code generated by Wasp). Wasp expects you to separate all external code into three folders to make it clear which runtime executes what:
- `src/server` - Contains the code executed on the server (i.e., in Node)
- `src/client` - Contains the code executed on the client (i.e., JS in the browser)
- `src/shared` - Contains the code you want to use on both the client and the server (e.g., runtime-independent utility functions)

You may be wondering what about the rest of the generated files (`tsconfig.json`
and `vite-env.d.ts`? Your IDE needs them to improve your development
experience (i.e., autocompletion, intellisense, etc.), so it's best to leave
them alone (for now).

:::note Typescript Support
Wasp supports Typescript out of the box but you are free to use JavaScript (js/jsx)
or TypeScript (ts/tsx) as you see fit. No extra configuration is needed!

We'll provide you with both JavaScript and TypeScript snippets for each feature we cover.

Code blocks have a toggle between vanilla 🍦 Javascript/JSX and Typescript/TSX.

To see how to get the most out of Wasp and TypeScript, take a look at [our
TypeScript doc](/docs/typescript). It contains a list of all TypeScript features
Wasp currently supports.
:::

Let's start with the `main.wasp` file, which introduces 3 new concepts:
[app](language/features.md#app),
[page](language/features.md#page) and
[route](language/features.md#route).

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app TodoApp { // Main declaration, defines a new web app.
  wasp: {
    version: "^0.11.0"
  },
  title: "Todo app" // Used as a browser tab title.
}

route RootRoute { path: "/", to: MainPage } // Render page MainPage on url `/`

page MainPage {
  // We specify that ReactJS implementation of our page can be found in
  // `src/client/MainPage.jsx` as a default export (uses standard js import syntax).
  // Use '@client' to reference files inside the src/client folder.
  component: import Main from "@client/MainPage.jsx"
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
app TodoApp { // Main declaration, defines a new web app.
  wasp: {
    version: "^0.11.0"
  },
  title: "Todo app" // Used as a browser tab title.
}

route RootRoute { path: "/", to: MainPage } // Render page MainPage on url `/`

page MainPage {
  // We specify that ReactJS implementation of our page can be found in
  // `src/client/MainPage.tsx` as a default export (uses standard JS import syntax).
  // Use '@client' to reference files inside the src/client folder.
  component: import Main from "@client/MainPage.tsx"
}
```
</TabItem>
</Tabs>

<ShowForTs>

:::warning Using TypeScript
If you are using Typescript, you will need to rename `MainPage.jsx` to `MainPage.tsx`.
:::
</ShowForTs>

Let's now take a look at that React component we referenced in the `page MainPage { ... }` declaration in `main.wasp`:
<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="src/client/MainPage.jsx"
import waspLogo from './waspLogo.png'
import './Main.css'

const MainPage = () => {
  ...
}
export default MainPage
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/client/MainPage.tsx"
import waspLogo from './waspLogo.png'
import './Main.css'

const MainPage = () => {
  ...
}
export default MainPage
```

</TabItem>
</Tabs>

As we can see, this is simply a functional React component that uses the CSS and Wasp logo files sitting next to it in the `src/client` dir.

This is all the code we need!
Wasp quietly takes care of everything else necessary to define, build, and run a web app.

:::tip
`wasp start` automatically picks up the changes you make and restarts the app, so keep it running.
:::

## Cleaning up

Let's make our first changes!

To prepare the clean slate for building the TodoApp, delete the files `Main.css`
and `waspLogo.png` from the `src/client/` folder (`src/shared` and `src/server`
are already clean). Wasp needs the `tsconfig.json` and `vite-env.d.ts` for
IDE support, so it's important to keep them.

Next, let's make the `MainPage` component much simpler:


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

At this point, you should see something like this:

<img alt="Todo App - Hello World"
     src={useBaseUrl('img/todo-app-hello-world.png')}
     style={{ border: "1px solid black" }}
/>

Ok, time to take the next step - implementing some real Todo app features!