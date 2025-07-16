---
title: Actions
---

import { Required } from '@site/src/components/Tag';
import { ShowForTs } from '@site/src/components/TsJsHelpers';
import SuperjsonNote from './\_superjson-note.md';

We'll explain what Actions are and how to use them. If you're looking for a detailed API specification, skip ahead to the [API Reference](#api-reference).

Actions are quite similar to [Queries](../../data-model/operations/queries.md), but with a key distinction: Actions are designed to modify and add data, while Queries are solely for reading data. Examples of Actions include adding a comment to a blog post, liking a video, or updating a product's price.

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

1. Declare the Action in Wasp using the `action` declaration.
2. Implement the Action's NodeJS functionality.

Once these two steps are completed, you can use the Action from anywhere in your code.

### Declaring Actions

To create an Action in Wasp, we begin with an `action` declaration. Let's declare two Actions - one for creating a task, and another for marking tasks as done:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```wasp title="main.wasp"
    // ...

    action createTask {
      fn: import { createTask } from "@src/actions.js"
    }

    action markTaskAsDone {
      fn: import { markTaskAsDone } from "@src/actions.js"
    }

    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```wasp title="main.wasp"
    // ...

    action createTask {
      fn: import { createTask } from "@src/actions.js"
    }

    action markTaskAsDone {
      fn: import { markTaskAsDone } from "@src/actions.js"
    }
    ```
  </TabItem>
</Tabs>

<small>
  If you want to know about all supported options for the `action` declaration, take a look at the [API Reference](#api-reference).
</small>

The names of Wasp Actions and their implementations don't necessarily have to match. However, to avoid confusion, we'll keep them the same.

:::info
You might have noticed that we told Wasp to import Action implementations that don't yet exist. Don't worry about that for now. We'll write the implementations imported from `actions.{js,ts}` in the next section.

It's a good idea to start with the high-level concept (the Action declaration in the Wasp file) and only then deal with the implementation details (the Action's implementation in JavaScript).
:::

After declaring a Wasp Action, two important things happen:

- Wasp **generates a server-side NodeJS function** that shares its name with the Action.

- Wasp **generates a client-side JavaScript function** that shares its name with the Action (e.g., `markTaskAsDone`).
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
      { id: 1, description: 'Buy some eggs', isDone: true },
      { id: 2, description: 'Make an omelette', isDone: false },
      { id: 3, description: 'Eat breakfast', isDone: false },
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
    import { type CreateTask, type MarkTaskAsDone } from 'wasp/server/operations'

    type Task = {
      id: number
      description: string
      isDone: boolean
    }

    // our "database"
    let nextId = 4
    const tasks = [
      { id: 1, description: 'Buy some eggs', isDone: true },
      { id: 2, description: 'Make an omelette', isDone: false },
      { id: 3, description: 'Eat breakfast', isDone: false },
    ]

    // You don't need to use the arguments if you don't need them
    export const createTask: CreateTask<Pick<Task, 'description'>, Task> = (
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
    export const markTaskAsDone: MarkTaskAsDone<Pick<Task, 'id'>, void> = (
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

    Wasp automatically generates the types `CreateTask` and `MarkTaskAsDone` based on the declarations in your Wasp file:

    - `CreateTask` is a generic type that Wasp automatically generated based on the Action declaration for `createTask`.
    - `MarkTaskAsDone` is a generic type that Wasp automatically generated based on the Action declaration for `markTaskAsDone`.

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
    }) satisfies GetFoo
    ```

    From the snippet above, TypeScript knows:

    1. The correct type for `context`.
    2. The Action's return type is `{ newFoo: Foo, message: string, returnedAt: Date }`.

    If you don't need the context, you can skip specifying the Action's type (and arguments):

    ```typescript
    const createFoo = () => {{ name: 'Foo', date: new Date() }}
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
    import { createTask, markTaskAsDone } from 'wasp/client/operations'

    // ...

    const newTask = await createTask({ description: 'Learn TypeScript' })
    await markTaskAsDone({ id: 1 })
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts
    import { createTask, markTaskAsDone } from 'wasp/client/operations'

    // TypeScript automatically infers the return values and type-checks
    // the payloads.
    const newTask = await createTask({ description: 'Keep learning TypeScript' })
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
    import React from 'react'
    // highlight-next-line
    import { useQuery, getTask, markTaskAsDone } from 'wasp/client/operations'

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
            {isDone ? 'Yes' : 'No'}
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
    import React from 'react'
    // highlight-next-line
    import { useQuery, getTask, markTaskAsDone } from 'wasp/client/operations'

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
            {isDone ? 'Yes' : 'No'}
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
    import { createTask, markTaskAsDone } from 'wasp/server/operations'

    const user = // Get an AuthUser object, e.g., from context.user

    const newTask = await createTask(
      { description: 'Learn TypeScript' },
      { user },
    )
    await markTaskAsDone({ id: 1 }, { user })
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts
    import { createTask, markTaskAsDone } from 'wasp/server/operations'

    const user = // Get an AuthUser object, e.g., from context.user

    // TypeScript automatically infers the return values and type-checks
    // the payloads.
    const newTask = await createTask(
      { description: 'Keep learning TypeScript' },
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
    import { HttpError } from 'wasp/server'

    export const createTask = async (args, context) => {
      throw new HttpError(
        403, // status code
        "You can't do this!", // message
        { foo: 'bar' } // data
      )
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="src/actions.ts"
    import { type CreateTask } from 'wasp/server/operations'
    import { HttpError } from 'wasp/server'

    export const createTask: CreateTask = async (args, context) => {
      throw new HttpError(
        403, // status code
        "You can't do this!", // message
        { foo: 'bar' } // data
      )
    }
    ```
  </TabItem>
</Tabs>

### Using Entities in Actions

In most cases, resources used in Actions will be [Entities](../../data-model/entities.md).
To use an Entity in your Action, add it to the `action` declaration in Wasp:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```wasp {4,9} title="main.wasp"

    action createTask {
      fn: import { createTask } from "@src/actions.js",
      entities: [Task]
    }

    action markTaskAsDone {
      fn: import { markTaskAsDone } from "@src/actions.js",
      entities: [Task]
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```wasp {4,9} title="main.wasp"

    action createTask {
      fn: import { createTask } from "@src/actions.js",
      entities: [Task]
    }

    action markTaskAsDone {
      fn: import { markTaskAsDone } from "@src/actions.js",
      entities: [Task]
    }
    ```
  </TabItem>
</Tabs>

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
    import { type CreateTask, type MarkTaskAsDone } from 'wasp/server/operations'
    import { type Task } from 'wasp/entities'

    // The 'args' object is the payload sent by the caller (most often from the client)
    export const createTask: CreateTask<Pick<Task, 'description'>, Task> = async (
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

    export const markTaskAsDone: MarkTaskAsDone<Pick<Task, 'id'>, void> = async (
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
3. `action` declarations in Wasp are mostly identical to `query` declarations. The only difference lies in the declaration's name.

## API Reference

### Declaring Actions in Wasp

The `action` declaration supports the following fields:

- `fn: ExtImport` <Required />

  The import statement of the Action's NodeJs implementation.

- `entities: [Entity]`

  A list of entities you wish to use inside your Action.
  For instructions on using Entities in Actions, take a look at [the guide](#using-entities-in-actions).

#### Example

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    Declaring the Action:

    ```wasp
    query createFoo {
        fn: import { createFoo } from "@src/actions.js"
        entities: [Foo]
    }
    ```

    Enables you to import and use it anywhere in your code (on the server or the client):

    ```js
    // Use it on the client
    import { createFoo } from 'wasp/client/operations'

    // Use it on the server
    import { createFoo } from 'wasp/server/operations'
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    Declaring the Action:

    ```wasp
    query createFoo {
        fn: import { createFoo } from "@src/actions.js"
        entities: [Foo]
    }
    ```

    Enables you to import and use it anywhere in your code (on the server or the client):

    ```ts
    // Use it on the client
    import { createFoo } from 'wasp/client/operations'

    // Use it on the server
    import { createFoo } from 'wasp/server/operations'
    ```

    As well as the following type import on the server:

    ```ts
    import { type CreateFoo } from 'wasp/server/operations'
    ```
  </TabItem>
</Tabs>

### Implementing Actions

The Action's implementation is a NodeJS function that takes two arguments (it can be an `async` function if you need to use the `await` keyword).
Since both arguments are positional, you can name the parameters however you want, but we'll stick with `args` and `context`:

1. `args` (type depends on the Action)

   An object containing the data **passed in when calling the Action** (e.g., filtering conditions).
   Check [the usage examples](#using-actions) to see how to pass this object to the Action.

2. `context` (type depends on the Action)

   An additional context object **passed into the Action by Wasp**. This object contains user session information, as well as information about entities. Check the [section about using entities in Actions](#using-entities-in-actions) to see how to use the entities field on the `context` object, or the [auth section](../../auth/overview#using-the-contextuser-object) to see how to use the `user` object.

<ShowForTs>
  After you [declare the Action](#declaring-actions), Wasp generates a generic type you can use when defining its implementation.
  For the Action declared as `createSomething`, the generated type is called `CreateSomething`:

  ```ts
  import { type CreateSomething } from 'wasp/server/operations'
  ```

  It expects two (optional) type arguments:

  1. `Input`

     The type of the `args` object (the Action's input payload). The default value is `never`.

  2. `Output`

     The type of the Action's return value (the Action's output payload). The default value is `unknown`.

  The defaults were chosen to make the type signature as permissive as possible. If don't want your Action to take/return anything, use `void` as a type argument.
</ShowForTs>

#### Example

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    The following Action:

    ```wasp
    action createFoo {
        fn: import { createFoo } from "@src/actions.js"
        entities: [Foo]
    }
    ```

    Expects to find a named export `createfoo` from the file `src/actions.js`

    ```js title="actions.js"
    export const createFoo = (args, context) => {
      // implementation
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    The following Action:

    ```wasp
    action createFoo {
        fn: import { createFoo } from "@src/actions.js"
        entities: [Foo]
    }
    ```

    Expects to find a named export `createfoo` from the file `src/actions.js`

    You can use the generated type `CreateFoo` and specify the Action's inputs and outputs using its type arguments.

    ```ts title="actions.ts"
    import { type CreateFoo } from 'wasp/server/operations'

    type Foo = // ...

    export const createFoo: CreateFoo<{ bar: string }, Foo> = (args, context) => {
      // implementation
    };
    ```

    In this case, the Action expects to receive an object with a `bar` field of type `string` (this is the type of `args`), and return a value of type `Foo` (this must match the type of the Action's return value).
  </TabItem>
</Tabs>

### The `useAction` Hook and Optimistic Updates

Make sure you understand how [Queries](../../data-model/operations/queries.md) and [Cache Invalidation](#cache-invalidation) work before reading this chapter.

When using Actions in components, you can enhance them with the help of the `useAction` hook. This hook comes bundled with Wasp, and is used for decorating Wasp Actions.
In other words, the hook returns a function whose API matches the original Action while also doing something extra under the hood (depending on how you configure it).

The `useAction` hook accepts two arguments:

- `actionFn` <Required />

  The Wasp Action (the client-side Action function generated by Wasp based on a Action declaration) you wish to enhance.

- `actionOptions`

  An object configuring the extra features you want to add to the given Action. While this argument is technically optional, there is no point in using the `useAction` hook without providing it (it would be the same as using the Action directly). The Action options object supports the following fields:

  - `optimisticUpdates`

    An array of objects where each object defines an [optimistic update](https://stackoverflow.com/a/33009713) to perform on the Query cache. To define an optimistic update, you must specify the following properties:

    - `getQuerySpecifier` <Required />

    A function returning the Query specifier (a value used to address the Query you want to update). A Query specifier is an array specifying the query function and arguments. For example, to optimistically update the Query used with `useQuery(fetchFilteredTasks, {isDone: true }]`, your `getQuerySpecifier` function would have to return the array `[fetchFilteredTasks, { isDone: true}]`. Wasp will forward the argument you pass into the decorated Action to this function (you can use the properties of the added/changed item to address the Query).

    - `updateQuery` <Required />

    The function used to perform the optimistic update. It should return the desired state of the cache. Wasp will call it with the following arguments:

    - `item` - The argument you pass into the decorated Action.
    - `oldData` - The currently cached value for the Query identified by the specifier.

:::caution
The `updateQuery` function must be a pure function. It must return the desired cache value identified by the `getQuerySpecifier` function and _must not_ perform any side effects.

Also, make sure you only update the Query caches affected by your Action causing the optimistic update (Wasp cannot yet verify this).

Finally, your implementation of the `updateQuery` function should work correctly regardless of the state of `oldData` (e.g., don't rely on array positioning). If you need to do something else during your optimistic update, you can directly use _react-query_'s lower-level API (read more about it [here](#advanced-usage)).
:::

Here's an example showing how to configure the Action `markTaskAsDone` that toggles a task's `isDone` status to perform an optimistic update:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```jsx title="src/pages/Task.jsx"
    import React from 'react'
    import {
      useQuery,
      useAction,
      getTask,
      markTaskAsDone,
    } from 'wasp/client/operations'

    const TaskPage = ({ id }) => {
      const { data: task } = useQuery(getTask, { id })
      // highlight-start
      const markTaskAsDoneOptimistically = useAction(markTaskAsDone, {
        optimisticUpdates: [
          {
            getQuerySpecifier: ({ id }) => [getTask, { id }],
            updateQuery: (_payload, oldData) => ({ ...oldData, isDone: true }),
          },
        ],
      })
      // highlight-end

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
            {isDone ? 'Yes' : 'No'}
          </p>
          {isDone || (
            <button onClick={() => markTaskAsDoneOptimistically({ id })}>
              Mark as done.
            </button>
          )}
        </div>
      )
    }

    export default TaskPage
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```tsx title="src/pages/Task.tsx"
    import React from 'react'
    import {
      useQuery,
      useAction,
      type OptimisticUpdateDefinition,
      getTask,
      markTaskAsDone,
    } from 'wasp/client/operations'

    type TaskPayload = Pick<Task, "id">;

    const TaskPage = ({ id }: { id: number }) => {
      const { data: task } = useQuery(getTask, { id });
      // Typescript automatically type-checks the payload type.
      // highlight-start
      const markTaskAsDoneOptimistically = useAction(markTaskAsDone, {
        optimisticUpdates: [
          {
            getQuerySpecifier: ({ id }) => [getTask, { id }],
            updateQuery: (_payload, oldData) => ({ ...oldData, isDone: true }),
          } as OptimisticUpdateDefinition<TaskPayload, Task>,
        ],
      });
      // highlight-end

      if (!task) {
        return <h1>"Loading"</h1>;
      }

      const { description, isDone } = task;
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
            <button onClick={() => markTaskAsDoneOptimistically({ id })}>
              Mark as done.
            </button>
          )}
        </div>
      );
    };

    export default TaskPage;
    ```
  </TabItem>
</Tabs>

#### Advanced usage

The `useAction` hook currently only supports specifying optimistic updates. You can expect more features in future versions of Wasp.

Wasp's optimistic update API is deliberately small and focuses exclusively on updating Query caches (as that's the most common use case). You might need an API that offers more options or a higher level of control. If that's the case, instead of using Wasp's `useAction` hook, you can use _react-query_'s `useMutation` hook and directly work with [their low-level API](https://tanstack.com/query/v4/docs/framework/react/guides/optimistic-updates).

If you decide to use _react-query_'s API directly, you will need access to Query cache key. Wasp internally uses this key but abstracts it from the programmer. Still, you can easily obtain it by accessing the `queryCacheKey` property on any Query:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js
    import { getTasks } from 'wasp/client/operations'

    const queryKey = getTasks.queryCacheKey
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts
    import { getTasks } from 'wasp/client/operations'

    const queryKey = getTasks.queryCacheKey
    ```
  </TabItem>
</Tabs>
