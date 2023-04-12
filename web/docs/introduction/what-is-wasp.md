---
title: What is Wasp?
---

import ImgWithCaption from '../../blog/components/ImgWithCaption'

We will give a brief overview of what Wasp is, how it works on a high level and when to use it.

## Wasp is a tool to build modern web applications

It is an opinionated way of building **full-stack web applications**. It takes care of all three
major parts of a web application: **client** (front-end), **server** (back-end) and **deployment**.

#### Works well with your existing stack
Wasp is not trying to do everything at once but rather focuses on the complexity
which arises from connecting all the parts of the stack (client, server, deployment) together.

Wasp is using **React**, **Node.js** and **Prisma** under the hood and relies on them to define web components and server queries and actions.

#### Wasp's secret sauce

At the core is the Wasp compiler which takes the Wasp config and your Javascript code and outputs the client app, server app and deployment code.

<!-- ![Wasp compilation diagram](/img/lp/wasp-compilation-diagram.png) -->

<ImgWithCaption
    source="/img/lp/wasp-compilation-diagram.png"
    caption="How the magic happens 🌈"
/>

The cool thing about having a compiler that understands your code is that it can do a lot of things for you. 

Define your app in the Wasp config and get:
- login and signup with Auth UI components,
- full-stack type safety,
- e-mail sending,
- async processing jobs,
- React Query powered data fetching,
- security best practices,
- and more. 

You don't need to write any code for these features, Wasp will take care of it for you 🤯 And what's even better, Wasp also maintains the code for you, so you don't have to worry about keeping up with the latest security best practices. As Wasp updates, so does your app.

### But what does it look like?

Let's say you want to build a web app that allows users to **create and share their favorite recipes**. 

You would start by defining your app in the Wasp file:

```c title="main.wasp"
app recepieApp {
  title: "My Recipes",
  wasp: {
    version: "^0.10.0"
  },
  auth: {
    methods: {
      google: {} // out-of-the-box auth with Google
    },
    onAuthFailedRedirectTo: "/login",
    onAuthSucceededRedirectTo: "/",
  },
}
```

Let's then add the data model for your recipes:

```c title="main.wasp"
// Use Prisma schema syntax to define your data model
entity User {=psl
  id          Int @id @default(autoincrement())
  name        String
  recipes     Recipe[]
psl=}

entity Recipe {=psl
  id          Int @id @default(autoincrement())
  title       String
  description String?
  userId      Int
  user        User @relation(fields: [userId], references: [id])
psl=}
```

Next, you would define some queries and actions...

```c title="main.wasp"
// Queries have automatic cache invalidation and are type-safe
query getRecipes {
  fn: import { getRecipes } from "@server/queries.js",
  entities: [Recipe],
}

// Actions are type-safe and can be used to perform side-effects
action addRecipe {
  fn: import { addRecipe } from "@server/actions.js",
  entities: [Recipe],
}
```

... which you would implement in your Javascript or Typescript code:

```ts title="src/server/queries.ts"
// Wasp compiler will generate types for you based on your data model
import { GetRecipes } from "@wasp/queries/types";
import { Recipe } from "@wasp/entities";

export const getRecipes: GetRecipes<{}, Recipe[]> = async (_args, context) => {
  // Use Prisma to query your database
  return context.entities.Recipe.findMany();
};
```

And then use it in your React component:

```tsx title="src/client/pages/RecipeListPage.tsx"
import getRecipes from "@wasp/queries/getRecipes";
import { useQuery } from "@wasp/queries";

export function Homepage({ user }: { user: User }) {
  // Due to full-stack type safety, `recipes` will be of type `Recipe[]` here
  const { data: recipes, isLoading } = useQuery(getRecipes);

  if (isLoading) {
    return <div>Loading...</div>;
  }

  return (
    <div>
      <h1>Recipes</h1>
      <ul>
        {recipes.map((recipe) => (
          <li key={recipe.id}>
            <Link to={`/recipes/${recipe.id}`}>{recipe.title}</Link>
          </li>
        ))}
      </ul>
    </div>
  );
}
```

And voila! We are listing all the recipes in our app 🎉

This was just a quick example to give you a taste of what Wasp is. For step by step tour through the most important Wasp features, check out the [Todo app tutorial](/docs/tutorials/todo-app).

## When to use Wasp
Wasp is addressing the same core problems that typical web app frameworks are addressing, and it in big part [looks, swims and quacks](https://en.wikipedia.org/wiki/Duck_test) like a web app framework.

### Best used for
- building full-stack web apps (like e.g. Airbnb or Asana)
- quickly starting a web app with industry best practices
- to be used alongside modern web dev stack (currently supported React and Node)

### Avoid using Wasp for
- building static/presentational websites
- to be used as a no-code solution
- to be a solve-it-all tool in a single language

## Wasp is a DSL

:::note
You don't need to know what a DSL is to use Wasp, but if you are curious, you can read more about it below.
:::

Wasp does not match typical expectations of a web app framework: it is not a set of libraries, it is instead a programming language that understands your code and can do a lot of things for you.

Wasp is a programming language, but a specific kind: it is specialized for a single purpose: **building modern web applications**. We call such languages *DSL*s (Domain Specific Language).

Other examples of *DSL*s that are often used today are e.g. *SQL* for databases and *HTML* for web page layouts.
The main advantage and reason why *DSL*s exist is that they need to do only one task (e.g. database queries)
so they can do it well and provide the best possible experience for the developer.

The same idea stands behind Wasp - a language that will allow developers to **build modern web applications with 10x less code and less stack-specific knowledge**.