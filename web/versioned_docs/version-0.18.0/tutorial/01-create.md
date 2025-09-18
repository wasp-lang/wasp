---
title: 1. Creating a New Project
---

import useBaseUrl from '@docusaurus/useBaseUrl';

:::info
You'll need to have the latest version of Wasp installed locally to follow this tutorial. If you haven't installed it yet, check out the [QuickStart](../quick-start) guide!
:::

In this section, we'll guide you through the process of creating a simple Todo app with Wasp. In the process, we'll take you through the most important and useful features of Wasp.

<img alt="How Todo App will work once it is done" src={useBaseUrl('img/todo-app-tutorial-intro.gif')} className="tutorial-image" />

<br />

<br />

If you get stuck at any point (or just want to chat), reach out to us on [Discord](https://discord.gg/rzdnErX) and we will help you!

You can find the complete code of the app we're about to build [here](https://github.com/wasp-lang/wasp/tree/release/examples/tutorials/TodoApp).

## Creating a Project

To setup a new Wasp project, run the following command in your terminal:

```sh
wasp new TodoApp -t minimal
```

<small>

We are using the `minimal` template because we're going to implement the app from scratch, instead of the more full-featured default template.
</small>

Enter the newly created directory and start the development server:

```sh
cd TodoApp
wasp start
```

`wasp start` will take a bit of time to start the server the first time you run it in a new project.

You will see log messages from the client, server, and database setting themselves up. When everything is ready, a new tab should open in your browser at `http://localhost:3000` with a simple placeholder page:

<img alt="Screenshot of the Wasp minimal starter app" src={useBaseUrl('img/wasp-new-screenshot.png')} className="tutorial-image" />

<br />

<br />

Wasp has generated for you the full front-end and back-end code of the app! Next, we'll take a closer look at how the project is structured.

## A note on supported languages

Wasp supports both JavaScript and TypeScript out of the box, but you are free to choose between or mix JavaScript and TypeScript as you see fit.

We'll provide you with both JavaScript and TypeScript code in this tutorial.
Code blocks will have a toggle to switch between vanilla JavaScript and TypeScript.

Try it out:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    :::note Welcome to JavaScript!

    You are now reading the JavaScript version of the docs. The site will remember your preference as you switch pages.

    You'll have a chance to change the language on every code snippet - both the snippets and the text will update accordingly.
    :::
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    :::note Welcome to TypeScript!

    You are now reading the TypeScript version of the docs. The site will remember your preference as you switch pages.

    You'll have a chance to change the language on every code snippet - both the snippets and the text will update accordingly.
    :::
  </TabItem>
</Tabs>
