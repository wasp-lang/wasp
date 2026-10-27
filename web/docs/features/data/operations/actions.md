---
title: "Actions: create and update data in Wasp"
sidebar_label: Actions
description: Actions create and update data in Wasp. Declare one in the spec, implement it in Node.js and call it from client or server code.
---

import { CardLink } from '@site/src/components/CardLink';
import SuperjsonNote from './_superjson-note.md';
import ReferencingCodeFromSrcNote from '../../../_referencing-code-from-src-note.md';

# Actions

We'll explain what Actions are and how to use them. If you're looking for a detailed API specification, skip ahead to the [API Reference](#api-reference).

Actions are quite similar to [Queries](./queries.md), but with a key distinction: Actions are designed to modify and add data, while Queries are solely for reading data. Examples of Actions include adding a comment to a blog post, liking a video, or updating a product's price.

Actions and Queries work together to keep data caches up-to-date.

:::tip
Actions are almost identical to Queries in terms of their API.
Therefore, if you're already familiar with Queries, you might find reading the entire guide repetitive.

We instead recommend skipping ahead and only reading [the differences between Queries and Actions](#differences-between-queries-and-actions), and consulting the [API Reference](#api-reference) as needed.
:::

## Working with Actions

Actions are declared in Wasp and implemented in NodeJS. Wasp runs Actions within the server's context, but it also generates code that allows you to call them from anywhere in your code (either client or server) using the same interface.

This means you don't have to worry about building an HTTP API for the Action, managing server-side request handling, or even dealing with client-side response handling and caching.
Instead, just focus on developing the business logic inside your Action, and let Wasp handle the rest!

To create an Action, you need to:

1. Declare the Action in Wasp using the `action` spec.
2. Implement the Action's NodeJS functionality.

Once these two steps are completed, you can use the Action from anywhere in your code.

### Specifying Actions

To create an Action in Wasp, we begin with an `action` spec. Let's declare two Actions - one for creating a task, and another for marking tasks as done:

```ts title="main.wasp.ts"
import { action, app } from "@wasp.sh/spec"
import { createTask, markTaskAsDone } from "./src/actions" with { type: "ref" }

export default app({
  // ...
  spec: [
    action(createTask),
    action(markTaskAsDone),
  ],
})
```

<small>
  If you want to know about all supported options for the `action` spec, take a look at the [API Reference](#api-reference).
</small>

<ReferencingCodeFromSrcNote />

:::info
You might have noticed that we told Wasp to import Action implementations that don't yet exist. Don't worry about that for now. We'll write the implementations imported from `actions.{js,ts}` in the next section.

It's a good idea to start with the high-level concept (the Action spec in the Wasp file) and only then deal with the implementation details (the Action's implementation in JavaScript).
:::

After declaring a Wasp Action, Wasp derives the Action's name from the function you pass to `action`. For example, `action(markTaskAsDone)` creates an Action named `markTaskAsDone`.

Two important things then happen:

- Wasp **generates a server-side NodeJS function** with the Action's name.

- Wasp **generates a client-side JavaScript function** with the Action's name (e.g., `markTaskAsDone`).
  This function takes a single optional argument - an object containing any serializable data you wish to use inside the Action.
  Wasp will send this object over the network and pass it into the Action's implementation as its first positional argument (more on this when we look at the implementations).
  Such an abstraction works thanks to an HTTP API route handler Wasp generates on the server, which calls the Action's NodeJS implementation under the hood.

Generating these two functions ensures a similar calling interface across the entire app (both client and server).

### Implementing Actions in Node

Now that we've declared the Action, what remains is to implement it. We've instructed Wasp to look for the Actions' implementations in the file `src/actions.{js,ts}`, so that's where we should export them from.

Here's how you might implement the previously declared Actions `createTask` and `markTaskAsDone`:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="src/actions.js"
    // our "database"
    let nextId = 4
    const tasks = [
      { id: 1, description: "Buy some eggs", isDone: true },
      { id: 2, description: "Make an omelette", isDone: false },
      { id: 3, description: "Eat breakfast", isDone: false },
    ]

    // You don't need to use the arguments if you don't need them
    export const createTask = (args) => {
      const newTask = {
        id: nextId,
        isDone: false,
        description: args.description,
      }
      nextId += 1
      tasks.push(newTask)
      return newTask
    }

    // The 'args' object is something sent by the caller (most often from the client)
    export const markTaskAsDone = (args) => {
      const task = tasks.find((task) => task.id === args.id)
      if (!task) {
        // We'll show how to properly handle such errors later
        return
      }
      task.isDone = true
    }
    ```

    <SuperjsonNote />
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="src/actions.ts"
    import { type CreateTask, type MarkTaskAsDone } from "wasp/server/operations"

    type Task = {
      id: number
      description: string
      isDone: boolean
    }

    // our "database"
    let nextId = 4
    const tasks = [
      { id: 1, description: "Buy some eggs", isDone: true },
      { id: 2, description: "Make an omelette", isDone: false },
      { id: 3, description: "Eat breakfast", isDone: false },
    ]

    // You don't need to use the arguments if you don't need them
    export const createTask: CreateTask<Pick<Task, "description">, Task> = (
      args
    ) => {
      const newTask = {
        id: nextId,
        isDone: false,
        description: args.description,
      }
      nextId += 1
      tasks.push(newTask)
      return newTask
    }

    // The 'args' object is something sent by the caller (most often from the client)
    export const markTaskAsDone: MarkTaskAsDone<Pick<Task, "id">, void> = (
      args
    ) => {
      const task = tasks.find((task) => task.id === args.id)
      if (!task) {
        // We'll show how to properly handle such errors later
        return
      }
      task.isDone = true
    }
    ```

    <SuperjsonNote />

    #### Type support for Actions

    Wasp automatically generates the types `CreateTask` and `MarkTaskAsDone` based on the specs in your Wasp file:

    - `CreateTask` is a generic type that Wasp automatically generated based on the Action spec for `createTask`.
    - `MarkTaskAsDone` is a generic type that Wasp automatically generated based on the Action spec for `markTaskAsDone`.

    Use these types to type the Action's implementation.
    It's optional but very helpful since doing so properly types the Action's context.

    In this case, TypeScript will know the `context.entities` object must include the `Task` entity.
    TypeScript also knows whether the `context` object includes user information (it depends on whether your Action uses auth).

    The generated types are generic and accept two optional type arguments: `Input` and `Output`.

    1. `Input` - The argument (the payload) received by the Action function.
    2. `Output` - The Action function's return type.

    Use these type arguments to type the Action's inputs and outputs.

    <details>
      <summary>Explanation for the example above</summary>

      The above code says that the Action `createTask` expects an object with the new task's description (its input type is `Pick<Task, 'description'>`) and returns the new task (its output type is `Task`).

      On the other hand, the Action `markTaskAsDone` expects an object of type `Pick<Task, 'id'>`. This type is derived from the `Task` entity type.

      If you don't care about typing the Action's inputs and outputs, you can omit both type arguments.
      TypeScript will then infer the most general types (`never` for the input and `unknown` for the output).

      Specifying `Input` or `Output` is completely optional, but we highly recommended it. Doing so gives you:

      - Type support for the arguments and the return value inside the implementation.
      - **Full-stack type safety**. We'll explore what this means when we discuss calling the Action from the client.
    </details>

    Read more about type support for implementing Actions in the [API Reference](#implementing-actions).

    :::tip Inferring the return type

    If don't want to explicitly type the Action's return value, the `satisfies` keyword tells TypeScript to infer it automatically:

    ```typescript
    const createFoo = (async (_args, context) => {
      const foo = await context.entities.Foo.create()
      return {
        newFoo: foo,
        message: "Here's your foo!",
        returnedAt: new Date(),
      }
    }) satisfies CreateFoo
    ```

    From the snippet above, TypeScript knows:

    1. The correct type for `context`.
    2. The Action's return type is `{ newFoo: Foo, message: string, returnedAt: Date }`.

    If you don't need the context, you can skip specifying the Action's type (and arguments):

    ```typescript
    const createFoo = () => ({ name: "Foo", date: new Date() })
    ```

    :::
  </TabItem>
</Tabs>

<small>
  For a detailed explanation of the Action definition API (more precisely, its arguments and return values), check the [API Reference](#api-reference).
</small>

### Using Actions

#### Using Actions on the client

To call an Action on the client, you can import it from `wasp/client/operations` and call it directly.

The usage doesn't depend on whether the Action is authenticated or not.
Wasp authenticates the logged-in user in the background.

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js
    import { createTask, markTaskAsDone } from "wasp/client/operations"

    // ...

    const newTask = await createTask({ description: "Learn TypeScript" })
    await markTaskAsDone({ id: 1 })
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts
    import { createTask, markTaskAsDone } from "wasp/client/operations"

    // TypeScript automatically infers the return values and type-checks
    // the payloads.
    const newTask = await createTask({ description: "Keep learning TypeScript" })
    await markTaskAsDone({ id: 1 })
    ```

    Wasp supports **automatic full-stack type safety**.
    You only need to specify the Action's type in its server-side definition, and the client code will automatically know its API payload types.
  </TabItem>
</Tabs>

When using Actions on the client, you'll most likely want to use them inside a component:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```jsx title="src/pages/Task.jsx"
    import React from "react"
    // highlight-next-line
    import { useQuery, getTask, markTaskAsDone } from "wasp/client/operations"

    export const TaskPage = ({ id }) => {
      const { data: task } = useQuery(getTask, { id })

      if (!task) {
        return <h1>"Loading"</h1>
      }

      const { description, isDone } = task
      return (
        <div>
          <p>
            <strong>Description: </strong>
            {description}
          </p>
          <p>
            <strong>Is done: </strong>
            {isDone ? "Yes" : "No"}
          </p>
          {isDone || (
            // highlight-next-line
            <button onClick={() => markTaskAsDone({ id })}>Mark as done.</button>
          )}
        </div>
      )
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```tsx title="src/pages/Task.tsx"
    import React from "react"
    // highlight-next-line
    import { useQuery, getTask, markTaskAsDone } from "wasp/client/operations"

    export const TaskPage = ({ id }: { id: number }) => {
      const { data: task } = useQuery(getTask, { id })

      if (!task) {
        return <h1>"Loading"</h1>
      }

      const { description, isDone } = task
      return (
        <div>
          <p>
            <strong>Description: </strong>
            {description}
          </p>
          <p>
            <strong>Is done: </strong>
            {isDone ? "Yes" : "No"}
          </p>
          {isDone || (
            // highlight-next-line
            <button onClick={() => markTaskAsDone({ id })}>Mark as done.</button>
          )}
        </div>
      )
    }
    ```
  </TabItem>
</Tabs>

Since Actions don't require reactivity, they are safe to use inside components without a hook. Still, Wasp provides comes with the `useAction` hook you can use to enhance actions. Read all about it in the [API Reference](#api-reference).

#### Using Actions on the server

Calling an Action on the server is similar to calling it on the client.

Here's what you have to do differently:

- Import Actions from `wasp/server/operations` instead of `wasp/client/operations`.
- Make sure you pass in a context object with the user to authenticated Actions.

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js
    import { createTask, markTaskAsDone } from "wasp/server/operations"

    const user = // Get an AuthUser object, e.g., from context.user

    const newTask = await createTask(
      { description: "Learn TypeScript" },
      { user },
    )
    await markTaskAsDone({ id: 1 }, { user })
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts
    import { createTask, markTaskAsDone } from "wasp/server/operations"

    const user = // Get an AuthUser object, e.g., from context.user

    // TypeScript automatically infers the return values and type-checks
    // the payloads.
    const newTask = await createTask(
      { description: "Keep learning TypeScript" },
      { user },
    )
    await markTaskAsDone({ id: 1 }, { user })
    ```
  </TabItem>
</Tabs>

### Error Handling

For security reasons, all exceptions thrown in the Action's NodeJS implementation are sent to the client as responses with the HTTP status code `500`, with all other details removed.
Hiding error details by default helps against accidentally leaking possibly sensitive information over the network.

If you do want to pass additional error information to the client, you can construct and throw an appropriate `HttpError` in your implementation:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="src/actions.js"
    import { HttpError } from "wasp/server"

    export const createTask = async (args, context) => {
      throw new HttpError(
        403, // status code
        "You can't do this!", // message
        { foo: "bar" } // data
      )
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="src/actions.ts"
    import { type CreateTask } from "wasp/server/operations"
    import { HttpError } from "wasp/server"

    export const createTask: CreateTask = async (args, context) => {
      throw new HttpError(
        403, // status code
        "You can't do this!", // message
        { foo: "bar" } // data
      )
    }
    ```
  </TabItem>
</Tabs>

To also set response headers, e.g. `Retry-After`, pass them in the options argument:

```js
throw new HttpError(429, "Too many requests", undefined, {
  headers: { "Retry-After": "60" },
})
```

### Using Entities in Actions

In most cases, resources used in Actions will be [Entities](../entities.md).
To use an Entity in your Action, add it to the `action` spec in Wasp:

```ts title="main.wasp.ts"
import { action, app } from "@wasp.sh/spec"
import { createTask, markTaskAsDone } from "./src/actions" with { type: "ref" }

export default app({
  // ...
  spec: [
    action(createTask, { entities: ["Task"] }),
    action(markTaskAsDone, { entities: ["Task"] }),
  ],
})
```

Wasp will inject the specified Entity into the Action's `context` argument, giving you access to the Entity's Prisma API.
Wasp invalidates frontend Query caches by looking at the Entities used by each Action/Query. Read more about Wasp's smart cache invalidation [here](#cache-invalidation).

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="src/actions.js"
    // The 'args' object is the payload sent by the caller (most often from the client)
    export const createTask = async (args, context) => {
      const newTask = await context.entities.Task.create({
        data: {
          description: args.description,
          isDone: false,
        },
      })
      return newTask
    }

    export const markTaskAsDone = async (args, context) => {
      await context.entities.Task.update({
        where: { id: args.id },
        data: { isDone: true },
      })
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="src/actions.ts"
    import { type CreateTask, type MarkTaskAsDone } from "wasp/server/operations"
    import { type Task } from "wasp/entities"

    // The 'args' object is the payload sent by the caller (most often from the client)
    export const createTask: CreateTask<Pick<Task, "description">, Task> = async (
      args,
      context
    ) => {
      const newTask = await context.entities.Task.create({
        data: {
          description: args.description,
          isDone: false,
        },
      })
      return newTask
    }

    export const markTaskAsDone: MarkTaskAsDone<Pick<Task, "id">, void> = async (
      args,
      context
    ) => {
      await context.entities.Task.update({
        where: { id: args.id },
        data: { isDone: true },
      })
    }
    ```

    Again, annotating the Actions is optional, but greatly improves **full-stack type safety**.
  </TabItem>
</Tabs>

The object `context.entities.Task` exposes `prisma.task` from [Prisma's CRUD API](https://www.prisma.io/docs/reference/tools-and-interfaces/prisma-client/crud).

## Cache Invalidation

One of the trickiest parts of managing a web app's state is making sure the data returned by the Queries is up to date.
Since Wasp uses _react-query_ for Query management, we must make sure to invalidate Queries (more specifically, their cached results managed by _react-query_) whenever they become stale.

It's possible to invalidate the caches manually through several mechanisms _react-query_ provides (e.g., refetch, direct invalidation).
However, since manual cache invalidation quickly becomes complex and error-prone, Wasp offers a faster and a more effective solution to get you started: **automatic Entity-based Query cache invalidation**.
Because Actions can (and most often do) modify the state while Queries read it, Wasp invalidates a Query's cache whenever an Action that uses the same Entity is executed.

For example, if the Action `createTask` and Query `getTasks` both use the Entity `Task`, executing `createTask` may cause the cached result of `getTasks` to become outdated. In response, Wasp will invalidate it, causing `getTasks` to refetch data from the server and update it.

In practice, this means that Wasp keeps the Queries "fresh" without requiring you to think about cache invalidation.

On the other hand, this kind of automatic cache invalidation can become wasteful (some updates might not be necessary) and will only work for Entities. If that's an issue, you can use the mechanisms provided by _react-query_ for now, and expect more direct support in Wasp for handling those use cases in a nice, elegant way.

If you wish to optimistically set cache values after performing an Action, you can do so using [optimistic updates](https://stackoverflow.com/a/33009713). Configure them using Wasp's [useAction hook](#the-useaction-hook-and-optimistic-updates). This is currently the only manual cache invalidation mechanism Wasps supports natively. For everything else, you can always rely on _react-query_.

## Differences Between Queries and Actions

Actions and Queries are two closely related concepts in Wasp. They might seem to perform similar tasks, but Wasp treats them differently, and each concept represents a different thing.

Here are the key differences between Queries and Actions:

1. Actions can (and often should) modify the server's state, while Queries are only permitted to read it. Wasp relies on you adhering to this convention when performing cache invalidations, so it's crucial to follow it.
2. Actions don't need to be reactive, so you can call them directly. However, Wasp does provide a [`useAction` React hook](#the-useaction-hook-and-optimistic-updates) for adding extra behavior to the Action (like optimistic updates).
3. `action` specs in Wasp are mostly identical to `query` specs. The only difference lies in the spec's name.

## API Reference

### Specifying Actions in Wasp Spec

<CardLink
  to="../../../api/@wasp.sh/spec/functions/action"
  kind="api"
  title="action"
  description="All the options for declaring an action in the Wasp spec."
/>

<CardLink
  to="../../../api/wasp/client/operations"
  kind="api"
  title="wasp/client/operations"
  description="The functions Wasp generates for calling your Actions on the client."
/>

<CardLink
  to="../../../api/wasp/server/operations"
  kind="api"
  title="wasp/server/operations"
  description="The functions Wasp generates for calling your Actions on the server, and the types for implementing them."
/>

### Implementing Actions

<CardLink
  to="../../../api/wasp/server/operations/type-aliases/AuthenticatedActionDefinition"
  kind="api"
  title="AuthenticatedActionDefinition"
  description="The arguments and return value of an Action's implementation, and the generic type Wasp generates for typing it."
/>

<CardLink
  to="../../../api/wasp/server/operations/type-aliases/UnauthenticatedActionDefinition"
  kind="api"
  title="UnauthenticatedActionDefinition"
  description="The same, for Actions that don't use auth."
/>

### The `useAction` Hook and Optimistic Updates

<CardLink
  to="../../../api/wasp/client/operations/functions/useAction"
  kind="api"
  title="useAction"
  description="All the arguments of the useAction hook, and how to use react-query's lower-level API directly."
/>

<CardLink
  to="../../../api/wasp/client/operations/type-aliases/OptimisticUpdateDefinition"
  kind="api"
  title="OptimisticUpdateDefinition"
  description="How to define an optimistic update, and the rules it must follow."
/>
