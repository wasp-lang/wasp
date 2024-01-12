---
title: 1. Creating a New Project
---

import useBaseUrl from '@docusaurus/useBaseUrl';

:::info
You'll need to have the latest version of Wasp installed locally to follow this tutorial. If you haven't installed it yet, check out the [QuickStart](../quick-start) guide!
:::

In this section, we'll guide you through the process of creating a simple Todo app with Wasp. In the process, we'll take you through the most important and useful features of Wasp.

<img alt="How Todo App will work once it is done"
src={useBaseUrl('img/todo-app-tutorial-intro.gif')}
style={{ border: "1px solid black" }}
/>
<br />
<br />

If you get stuck at any point (or just want to chat), reach out to us on [Discord](https://discord.gg/rzdnErX) and we will help you!

You can find the complete code of the app we're about to build [here](https://github.com/wasp-lang/wasp/tree/release/examples/tutorials/TodoApp).

:::tip See Wasp In Action
Prefer videos? We have a YouTube tutorial whick walks you through building this Todo app step by step. [Check it out here!](https://youtu.be/R8uOu6ZEr5s).

We've also set up an in-browser dev environment for you on Gitpod which allows you to view and edit the completed app with no installation required.

<p align="center">
     <a href="https://gitpod.io/#https://github.com/wasp-lang/gitpod-template">
          <img src="https://gitpod.io/button/open-in-gitpod.svg" />
     </a>
</p>
:::

## Creating a Project

To setup a new Wasp project, run the following command in your terminal

```sh
$ wasp new TodoApp
```

Enter the newly created directory and start the development server:

```sh
$ cd TodoApp
$ wasp start
```

:::note
`wasp start` will take a bit of time to start the server the first time you run it in a new project.
:::

You will see log messages from the client, server, and database setting themselves up. When everything is ready, a new tab should open in your browser at `http://localhost:3000` with a simple placeholder plage:

<img alt="Screenshot of new Wasp app"
src={useBaseUrl('img/wasp-new-screenshot.png')}
height="400px"
style={{ border: "1px solid black" }}
/>
<br />
<br />

Wasp has generated for you the full front-end and back-end code the app! Next, we'll take a closer look at how the project is structured.
