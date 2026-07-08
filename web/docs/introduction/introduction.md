---
title: Introduction
slug: /
---

import { ImgWithCaption } from '@site/blog/components/ImgWithCaption'
import { HiddenLLMHelper } from '@site/src/components/HiddenLLMHelper'

<HiddenLLMHelper />

:::note
If you are looking for the installation instructions, check out the [Quick Start](./quick-start.md) section.
:::

We will give a brief overview of what Wasp is, how it works on a high level and when to use it.

## Wasp is a tool to build modern web applications

It is an opinionated way of building **full-stack web applications**. It takes care of all three
major parts of a web application: **client** (front-end), **server** (back-end) and **database**.

### Works well with your existing stack

Wasp is not trying to do everything at once but rather focuses on the complexity that arises from connecting all the parts of the stack (client, server, database, deployment).

Wasp is using **React**, **Node.js** and **Prisma** under the hood and relies on them to define web components and server queries and actions.

### Wasp's secret sauce

At the core is the Wasp compiler which takes the Wasp Spec and your Javascript code and outputs the client app, server app and deployment code.

<!-- ![Wasp compilation diagram](/img/lp/wasp-compilation-diagram.png) -->

<ImgWithCaption source="/img/lp/wasp-compilation-diagram.png" caption="How the magic happens 🌈" />

The cool thing about having a compiler that understands your code is that it can do a lot of things for you.

Define your app in the Wasp file and get:

- login and signup with Auth UI components,
- full-stack type safety,
- e-mail sending,
- async processing jobs,
- React Query powered data fetching,
- security best practices,
- and more.

You don't need to write any code for these features, Wasp will take care of it for you 🤯 And what's even better, Wasp also maintains the code for you, so you don't have to worry about keeping up with the latest security best practices. As Wasp updates, so does your app.

## So what does the code look like?

Let's say you want to build a web app that allows users to **create and share their favorite recipes**.

Let's start with the `main.wasp.ts` file: it is the central spec file of your app, where you describe the app from the high level.

Let's give our app a title and let's immediately turn on the full-stack authentication via username and password:

```ts title="main.wasp.ts"
import { app, page, route } from "@wasp.sh/spec"
import { LoginPage } from "./src/pages/LoginPage" with { type: "ref" }

export default app({
  name: "RecipeApp",
  wasp: { version: "{latestWaspVersion}" },
  title: "My Recipes",
  head: ["<link rel='icon' href='/favicon.ico' />"],
  auth: {
    methods: { usernameAndPassword: {} },
    onAuthFailedRedirectTo: route("LoginRoute", "/login", page(LoginPage)),
    userEntity: "User",
  },
  // ...
})
```

The `route(...)` we passed to `onAuthFailedRedirectTo` is registered automatically because `auth` references it, so we don't have to list it in the app's `spec` array (just like pages passed to `route()`).

Let's then add the data models for your recipes. Wasp understands and uses the models from the `schema.prisma` file. We will want to have Users and Users can own Recipes:

```prisma title="schema.prisma"
...

// Data models are defined using Prisma Schema Language.
model User {
  id          Int @id @default(autoincrement())
  recipes     Recipe[]
}

model Recipe {
  id          Int @id @default(autoincrement())
  title       String
  description String?
  userId      Int
  user        User @relation(fields: [userId], references: [id])
}
```

Next, let's define how to do something with these data models!

We do that by defining Operations, in this case, a Query `getRecipes` and Action `addRecipe`,
which are in their essence Node.js functions that execute on the server and can, thanks to Wasp, very easily be called from the client.

First, we define these Operations in our `main.wasp.ts` file, so Wasp knows about them and can "beef them up":

```ts title="main.wasp.ts"
import { action, app, query } from "@wasp.sh/spec"
import { getRecipes, addRecipe } from "./src/recipe/operations" with { type: "ref" }

export default app({
  // ...
  spec: [
    // ...
    // Queries have automatic cache invalidation and are type-safe.
    query(getRecipes, { entities: ["Recipe"] }),
    // Actions are type-safe and can be used to perform side-effects.
    action(addRecipe, { entities: ["Recipe"] }),
  ],
})
```

... and then implement them in our Javascript (or TypeScript) code (we show just the query here, using TypeScript):

```ts title="src/recipe/operations.ts"
// Wasp generates the types for you.
import { type GetRecipes } from "wasp/server/operations";
import { type Recipe } from "wasp/entities";

export const getRecipes: GetRecipes<{}, Recipe[]> = async (_args, context) => {
  return context.entities.Recipe.findMany( // Prisma query
    { where: { user: { id: context.user.id } } }
  );
};

export const addRecipe ...
```

Now we can very easily use these in our React components!

For the end, let's create a home page of our app.

First, we define it in `main.wasp.ts`:

```ts title="main.wasp.ts"
import { app, page, route } from "@wasp.sh/spec"
import { HomePage } from "./src/pages/HomePage" with { type: "ref" }

export default app({
  // ...
  spec: [
    // ...
    route("HomeRoute", "/", page(HomePage, {
      authRequired: true, // Will send user to /login if not authenticated.
    })
    ),
  ],
})
```

and then implement it as a React component in JS/TS (that calls the Operations we previously defined):

```tsx title="src/pages/HomePage.tsx"
import { useQuery, getRecipes } from "wasp/client/operations"
import { type User } from "wasp/entities"

export function HomePage({ user }: { user: User }) {
  // Due to full-stack type safety, `recipes` will be of type `Recipe[]` here.
  const { data: recipes, isLoading } = useQuery(getRecipes) // Calling our query here!

  if (isLoading) {
    return <div>Loading...</div>
  }

  return (
    <div>
      <h1>Recipes</h1>
      <ul>
        {recipes
          ? recipes.map((recipe) => (
              <li key={recipe.id}>
                <div>{recipe.title}</div>
                <div>{recipe.description}</div>
              </li>
            ))
          : "No recipes defined yet!"}
      </ul>
    </div>
  )
}
```

And voila! We are listing all the recipes in our app 🎉

This was just a quick example to give you a taste of what Wasp is. For step by step tour through the most important Wasp features, check out the [Todo App tutorial](../tutorial/01-create.md).

:::note
Above we skipped implementing the `LoginPage` component and defining a `/signup` page to keep the example a bit shorter, but those are very simple to do by using Wasp's Auth UI feature.
:::

## When to use Wasp

Wasp addresses the same core problems that typical web app frameworks are addressing, and it in big part [looks, swims and quacks](https://en.wikipedia.org/wiki/Duck_test) like a web app framework.

### Best used for

- building full-stack web apps (like e.g. Airbnb or Asana)
- quickly starting a web app with industry best practices
- to be used alongside modern web dev stack (React and Node.js are currently supported)

### Avoid using Wasp for

- building static/presentational websites
- to be used as a no-code solution
- to be a solve-it-all tool in a single language

## Wasp is a spec-driven framework

Wasp does not match typical expectations of a web app framework: it is not just a set of libraries. You describe your app in the `main.wasp.ts` spec file, and the compiler uses that spec together with your React, Node.js, and Prisma code to generate the application structure and glue code.

This spec-driven approach lets Wasp focus on one purpose: **building modern web applications with 10x less code and less stack-specific knowledge**.
