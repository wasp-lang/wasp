---
id: 01-creating-new-project
title: "Creating new project"
---

import useBaseUrl from '@docusaurus/useBaseUrl';

Run the following command in your terminal to create a new Wasp project:
```shell-session
wasp new TodoApp
```
Enter the created directory and run:
```shell-session
cd TodoApp
wasp start
```
You have just run your app in the development mode!

:::note
`wasp start` might take a little bit longer, due to the first time setup.
:::

You will be seeing a lot of different output from the client, server and database setting themselves up.
Once ready, a new tab should open in your browser at `http://localhost:3000`, with simple placeholder page:

<img alt="Screenshot of new Wasp app"
     src={useBaseUrl('img/wasp-new-screenshot.png')}
     style={{ border: "1px solid black" }}
     height="400px"
/>

We just set the foundations of our app! We don't have yet the features to show it, but Wasp has already generated for us the full front-end and back-end code of the app. Take a peek at `TodoApp/.wasp/out` if you are curious and see how it looks like!

## Taking a closer look at the code

Let's inspect Wasp project that we just created:
```bash
TodoApp/
├── main.wasp # Here goes our Wasp code.
├── src/      # Here goes our (external) JS/CSS/HTML/... code.
│   ├──client/
│   │   └── MainPage.js
│   │   └── Main.css
│   │   └── waspLogo.png
│   │   └── tsconfig.json
│   ├──server/
│   │   └── tsconfig.json
│   └──shared/
|       └── tsconfig.json
├── .env.server
├── .env.client
├── .gitignore
└── .wasproot
```

:::note Typescript Support
We've introduced Typescript support in Beta version 0.7.0, but you are free to use js/jsx or ts/tsx syntax as you see fit.

For this tutorial, we will simply use vanilla Javascript and JSX syntax 🍦
:::

Let's start with the `main.wasp` file, which introduces 3 new concepts:
[app](language/features.md#app),
[page](language/features.md#page) and
[route](language/features.md#route).

```c title="main.wasp"
app TodoApp { // Main declaration, defines a new web app.
  title: "Todo app" // Used as a browser tab title.
}

route RootRoute { path: "/", to: MainPage } // Render page MainPage on url `/` (default url).

page MainPage {
  // We specify that ReactJS implementation of our page can be
  // found in `client/MainPage.js` as a default export (uses standard
  // js import syntax).
  component: import Main from "@client/MainPage.js"
}
```

And now to that React component we referenced in the `page MainPage { ... }` declaration in `main.wasp`:
```jsx title="client/MainPage.js"
import React from 'react'
import waspLogo from './waspLogo.png'
import './Main.css'

const MainPage = () => {
  ...
}
export default MainPage
```
As we can see, this is just a simple functional React component, using the CSS and Wasp logo files that are next to it.

This is all the code!
Wasp in the background takes care of everything else needed to define, build and run a web app.

:::tip
`wasp start` automatically picks up the changes you make and restarts the app, so keep it running.
:::

## Cleaning up

Let's make our first changes!
To prepare the clean slate for building the TodoApp, delete all the files from `client/` dir except for `MainPage.js`, and let's also make `MainPage` component much simpler:

```jsx title="client/MainPage.js"
import React from 'react'

const MainPage = () => {
  return <div> Hello world! </div>
}
export default MainPage
```

At this point, you should be seeing something like

<img alt="Todo App - Hello World"
     src={useBaseUrl('img/todo-app-hello-world.png')}
     style={{ border: "1px solid black" }}
/>

Ok, next step, some real Todo app features!
