---
title: Queries
---

import { Required } from '@site/src/components/Required';
import { ShowForTs } from '@site/src/components/TsJsHelpers';
import SuperjsonNote from './\_superjson-note.md';

We'll explain what Queries are and how to use them. If you're looking for a detailed API specification, skip ahead to the [API Reference](#api-reference).

You can use Queries to fetch data from the server. They shouldn't modify the server's state.
Fetching all comments on a blog post, a list of users that liked a video, information about a single product based on its ID... All of these are perfect use cases for a Query.

:::tip
Queries are fairly similar to Actions in terms of their API.
Therefore, if you're already familiar with Actions, you might find reading the entire guide repetitive.

We instead recommend skipping ahead and only reading [the differences between Queries and Actions](/docs/data-model/operations/actions#differences-between-queries-and-actions), and consulting the [API Reference](#api-reference) as needed.
:::

## Working with Queries

You declare queries in the `.wasp` file and implement them using NodeJS. Wasp not only runs these queries within the server's context but also creates code that enables you to call them from any part of your codebase, whether it's on the client or server side.

This means you don't have to build an HTTP API for your query, manage server-side request handling, or even deal with client-side response handling and caching.
Instead, just concentrate on implementing the business logic inside your query, and let Wasp handle the rest!

To create a Query, you must:

1. Declare the Query in Wasp using the `query` declaration.
2. Define the Query's NodeJS implementation.

After completing these two steps, you'll be able to use the Query from any point in your code.

### Declaring Queries

To create a Query in Wasp, we begin with a `query` declaration.

Let's declare two Queries - one to fetch all tasks, and another to fetch tasks based on a filter, such as whether a task is done:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...

query getAllTasks {
  fn: import { getAllTasks } from "@server/queries.js"
}

query getFilteredTasks {
  fn: import { getFilteredTasks } from "@server/queries.js"
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

query getAllTasks {
  fn: import { getAllTasks } from "@server/queries.js"
}

query getFilteredTasks {
  fn: import { getFilteredTasks } from "@server/queries.js"
}
```

:::warning
Even though you are using TypeScript and plan to implement this Query in `src/server/queries.ts`, you still need to import it using a `.js` extension. Wasp internally uses `esnext` module resolution, which requires importing all files with a `.js` extension. This is only needed when importing `@server` files.

Read more about ES modules in TypeScript [here](https://www.typescriptlang.org/docs/handbook/esm-node.html). If you're interested in the discussion and the reasoning behind this, read about it [in this GitHub issue](https://github.com/microsoft/TypeScript/issues/33588).
:::

</TabItem>
</Tabs>

<small>

If you want to know about all supported options for the `query` declaration, take a look at the [API Reference](#api-reference).

</small>

The names of Wasp Queries and their implementations don't need to match, but we'll keep them the same to avoid confusion.

:::info
You might have noticed that we told Wasp to import Query implementations that don't yet exist. Don't worry about that for now. We'll write the implementations imported from `queries.{js,ts}` in the next section.

It's a good idea to start with the high-level concept (i.e., the Query declaration in the Wasp file) and only then deal with the implementation details (i.e., the Query's implementation in JavaScript).
:::

After declaring a Wasp Query, two important things happen:

- Wasp **generates a server-side NodeJS function** that shares its name with the Query.

- Wasp **generates a client-side JavaScript function** that shares its name with the Query (e.g., `getFilteredTasks`).
  This function takes a single optional argument - an object containing any serializable data you wish to use inside the Query.
  Wasp will send this object over the network and pass it into the Query's implementation as its first positional argument (more on this when we look at the implementations).
  Such an abstraction works thanks to an HTTP API route handler Wasp generates on the server, which calls the Query's NodeJS implementation under the hood.

Generating these two functions ensures a uniform calling interface across the entire app (both client and server).

### Implementing Queries in Node

Now that we've declared the Query, what remains is to implement it.
We've instructed Wasp to look for the Queries' implementations in the file `src/server/queries.{js,ts}`, so that's where we should export them from.

Here's how you might implement the previously declared Queries `getAllTasks` and `getFilteredTasks`:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/server/queries.js"
// our "database"
const tasks = [
  { id: 1, description: 'Buy some eggs', isDone: true },
  { id: 2, description: 'Make an omelette', isDone: false },
  { id: 3, description: 'Eat breakfast', isDone: false },
]

// You don't need to use the arguments if you don't need them
export const getAllTasks = () => {
  return tasks
}

// The 'args' object is something sent by the caller (most often from the client)
export const getFilteredTasks = (args) => {
  const { isDone } = args
  return tasks.filter((task) => task.isDone === isDone)
}
```

<SuperjsonNote />

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/server/queries.ts"
import { GetAllTasks, GetFilteredTasks } from '@wasp/queries/types'

type Task = {
  id: number
  description: string
  isDone: boolean
}

// our "database"
const tasks: Task[] = [
  { id: 1, description: 'Buy some eggs', isDone: true },
  { id: 2, description: 'Make an omelette', isDone: false },
  { id: 3, description: 'Eat breakfast', isDone: false },
]

// You don't need to use the arguments if you don't need them
export const getAllTasks: GetAllTasks<void, Task[]> = () => {
  return tasks
}

// The 'args' object is something sent by the caller (most often from the client)
export const getFilteredTasks: GetFilteredTasks<
  Pick<Task, 'isDone'>,
  Task[]
> = (args) => {
  const { isDone } = args
  return tasks.filter((task) => task.isDone === isDone)
}
```

Wasp automatically generates the types `GetTasks` and `GetFilteredTasks` based on your Wasp file's declarations:

- `GetTasks` is a generic type automatically generated by Wasp, based on the Query declaration for `getTasks`.
- `GetFilteredTasks` is also a generic type automatically generated by Wasp, based on the Query declaration for `getFilteredTasks`.

You can utilize these types to define the input and output types for your Query.

For example, the Query `getTasks` doesn't expect any arguments (its input type is `void`), but it does return a list of tasks (its output type is `Task[]`).

On the other hand, the Query `getFilteredTasks` expects an object of type `{ isDone: boolean }`. This type is derived from the `Task` type.

While annotating the Queries is optional, it's highly recommended. Doing so enables **full-stack type safety**. We'll explore what this means when we discuss calling the Query from the client.

<SuperjsonNote />

</TabItem>
</Tabs>

<small>

For a detailed explanation of the Query definition API (i.e., arguments and return values), check the [API Reference](#api-reference).

</small>

### Using Queries

To use a Query, you can import it from `@wasp` and call it directly. As mentioned, the usage doesn't change depending on whether you're on the server or the client:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```javascript
import getAllTasks from '@wasp/queries/getAllTasks.js'
import getFilteredTasks from '@wasp/queries/getFilteredTasks.js'

// ...

const allTasks = await getAllTasks()
const doneTasks = await getFilteredTasks({ isDone: true })
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```typescript
import getAllTasks from '@wasp/queries/getAllTasks.js'
import getFilteredTasks from '@wasp/queries/getFilteredTasks.js'

// TypeScript automatically infers the return values and type-checks
// the payloads.
const allTasks = await getAllTasks()
const doneTasks = await getFilteredTasks({ isDone: true })
```

</TabItem>
</Tabs>

#### The `useQuery` hook

When using Queries on the client, you can make them reactive with the `useQuery` hook.
This hook comes bundled with Wasp and is a thin wrapper around the `useQuery` hook from [_react-query_](https://github.com/tannerlinsley/react-query). The only difference is that you don't need to supply the key - Wasp handles this for you automatically.

Here's an example of calling the Queries using the `useQuery` hook:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title=src/client/MainPage.jsx
import React from 'react'
import { useQuery } from '@wasp/queries'
import getAllTasks from '@wasp/queries/getAllTasks'
import getFilteredTasks from '@wasp/queries/getFilteredTasks'

const MainPage = () => {
  const { data: allTasks, error: error1 } = useQuery(getAllTasks)
  const { data: doneTasks, error: error2 } = useQuery(getFilteredTasks, {
    isDone: true,
  })

  if (error1 !== null || error2 !== null) {
    return <div>There was an error</div>
  }

  return (
    <div>
      <h2>All Tasks</h2>
      {allTasks && allTasks.length > 0
        ? allTasks.map((task) => <Task key={task.id} {...task} />)
        : 'No tasks'}

      <h2>Finished Tasks</h2>
      {doneTasks && doneTasks.length > 0
        ? doneTasks.map((task) => <Task key={task.id} {...task} />)
        : 'No finished tasks'}
    </div>
  )
}

const Task = ({ description, isDone }: Task) => {
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
    </div>
  )
}

export default MainPage
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title=src/client/MainPage.tsx
import React from 'react'
import { Task } from '@wasp/entities'
import { useQuery } from '@wasp/queries'
import getAllTasks from '@wasp/queries/getAllTasks'
import getFilteredTasks from '@wasp/queries/getFilteredTasks'

const MainPage = () => {
  // TypeScript will automatically infer and type-check payload types.
  const { data: allTasks, error: error1 } = useQuery(getAllTasks)
  const { data: doneTasks, error: error2 } = useQuery(getFilteredTasks, {
    isDone: true,
  })

  if (error1 !== null || error2 !== null) {
    return <div>There was an error</div>
  }

  return (
    <div>
      <h2>All Tasks</h2>
      {allTasks && allTasks.length > 0
        ? allTasks.map((task) => <Task key={task.id} {...task} />)
        : 'No tasks'}

      <h2>Finished Tasks</h2>
      {doneTasks && doneTasks.length > 0
        ? doneTasks.map((task) => <Task key={task.id} {...task} />)
        : 'No finished tasks'}
    </div>
  )
}

const Task = ({ description, isDone }: Task) => {
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
    </div>
  )
}

export default MainPage
```

Notice how you don't need to annotate the Query's return value type. Wasp automatically infers the from the Query's backend implementation. This is **full-stack type safety**: the types on the client always match the types on the server.

</TabItem>
</Tabs>

<small>

For a detailed specification of the `useQuery` hook, check the [API Reference](#api-reference).

</small>

### Error Handling

For security reasons, all exceptions thrown in the Query's NodeJS implementation are sent to the client as responses with the HTTP status code `500`, with all other details removed.
Hiding error details by default helps against accidentally leaking possibly sensitive information over the network.

If you do want to pass additional error information to the client, you can construct and throw an appropriate `HttpError` in your implementation:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title=src/server/queries.js
import HttpError from '@wasp/core/HttpError.js'

export const getAllTasks = async (args, context) => {
  throw new HttpError(
    403, // status code
    "You can't do this!", // message
    { foo: 'bar' } // data
  )
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title=src/server/queries.ts
import { GetAllTasks } from '@wasp/queries/types'
import HttpError from '@wasp/core/HttpError.js'

export const getAllTasks: GetAllTasks = async (args, context) => {
  throw new HttpError(
    403, // status code
    "You can't do this!", // message
    { foo: 'bar' } // data
  )
}
```

</TabItem>
</Tabs>

If the status code is `4xx`, the client will receive a response object with the corresponding `message` and `data` fields, and it will rethrow the error (including these fields).
To prevent information leakage, the server won't forward these fields for any other HTTP status codes.

### Using Entities in Queries

In most cases, resources used in Queries will be [Entities](/docs/data-model/entities.md).
To use an Entity in your Query, add it to the `query` declaration in Wasp:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp {4,9} title="main.wasp"

query getAllTasks {
  fn: import { getAllTasks } from "@server/queries.js",
  entities: [Task]
}

query getFilteredTasks {
  fn: import { getFilteredTasks } from "@server/queries.js",
  entities: [Task]
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp {4,9} title="main.wasp"

query getAllTasks {
  fn: import { getAllTasks } from "@server/queries.js",
  entities: [Task]
}

query getFilteredTasks {
  fn: import { getFilteredTasks } from "@server/queries.js",
  entities: [Task]
}
```

</TabItem>
</Tabs>

Wasp will inject the specified Entity into the Query's `context` argument, giving you access to the Entity's Prisma API:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/server/queries.js"
export const getAllTasks = async (args, context) => {
  return context.entities.Task.findMany({})
}

export const getFilteredTasks = async (args, context) => {
  return context.entities.Task.findMany({
    where: { isDone: args.isDone },
  })
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/server/queries.ts"
import { Task } from '@wasp/entities'
import { GetAllTasks, GetFilteredTasks } from '@wasp/queries/types'

export const getAllTasks: GetAllTasks<void, Task[]> = async (args, context) => {
  return context.entities.Task.findMany({})
}

export const getFilteredTasks: GetFilteredTasks<
  Pick<Task, 'isDone'>,
  Task[]
> = async (args, context) => {
  return context.entities.Task.findMany({
    where: { isDone: args.isDone },
  })
}
```

Again, annotating the Queries is optional, but greatly improves **full-stack type safety**.

</TabItem>
</Tabs>

The object `context.entities.Task` exposes `prisma.task` from [Prisma's CRUD API](https://www.prisma.io/docs/reference/tools-and-interfaces/prisma-client/crud).

## API Reference

### Declaring Queries

The `query` declaration supports the following fields:

- `fn: ServerImport` <Required />

  The import statement of the Query's NodeJs implementation.

- `entities: [Entity]`

  A list of entities you wish to use inside your Query.
  For instructions on using Entities in Queries, take a look at [the guide](#using-entities-in-queries).

#### Example

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

Declaring the Query:

```wasp
query getFoo {
    fn: import { getFoo } from "@server/queries.js"
    entities: [Foo]
}
```

Enables you to import and use it anywhere in your code (on the server or the client):

```js
import getFoo from '@wasp/queries/getFoo'
```

</TabItem>
<TabItem value="ts" label="TypeScript">

Declaring the Query:

```wasp
query getFoo {
    fn: import { getFoo } from "@server/queries.js"
    entities: [Foo]
}
```

Enables you to import and use it anywhere in your code (on the server or the client):

```ts
import getFoo from '@wasp/queries'
```

And also creates a type you can import on the server:

```ts
import type { GetFoo } from '@wasp/queries/types'
```

</TabItem>
</Tabs>

### Implementing Queries

The Query's implementation is a NodeJS function that takes two arguments (it can be an `async` function if you need to use the `await` keyword).
Since both arguments are positional, you can name the parameters however you want, but we'll stick with `args` and `context`:

1. `args` (type depends on the Query)

   An object containing the data **passed in when calling the query** (e.g., filtering conditions).
   Check [the usage examples](#using-queries) to see how to pass this object to the Query.

2. `context` (type depends on the Query)

   An additional context object **passed into the Query by Wasp**. This object contains user session information, as well as information about entities. Check the [section about using entities in Queries](#using-entities-in-queries) to see how to use the entities field on the `context` object, or the [auth section](/docs/auth/overview#using-the-contextuser-object) to see how to use the `user` object.

<ShowForTs>

Afer you [declare the query](#declaring-queries), Wasp generates a generic type you can use when defining its implementation.
For the Query declared as `getSomething`, the generated type is called `GetSomething`:

```ts
import { GetSomething } from '@wasp/queries/types'
```

It expects two (optional) type arguments:

1.  `Input`

    The type of the `args` object (i.e., the Query's input payload). The default value is `never`.

2.  `Output`

    The type of the Query's return value (i.e., the Query's output payload). The default value is `unknown`.

The defaults were chosen to make the type signature as permissive as possible. If don't want your Query to take/return anything, use `void` as a type argument.

</ShowForTs>

#### Example

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

The following Query:

```wasp
query getFoo {
    fn: import { getFoo } from "@server/queries.js"
    entities: [Foo]
}
```

Expects to find a named export `getFoo` from the file `src/server/queries.js`

```js title=queries.js
export const getFoo = (args, context) => {
  // implementation
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

The following Query:

```wasp
query getFoo {
    fn: import { getFoo } from "@server/queries.js"
    entities: [Foo]
}
```

Expects to find a named export `getFoo` from the file `src/server/queries.js`

You can use the generated type `GetFoo` and specify the Query's inputs and outputs using its type arguments.

```ts title=queries.ts
import { GetFoo } from "@wasp/queries/types";

type Foo = // ...

export const getFoo: GetFoo<{ id: number }, Foo> = (args, context) => {
  // implementation
};
```

In this case, the Query expects to receive an object with an `id` field of type `number` (this is the type of `args`), and return a value of type `Foo` (this must match the type of the Query's return value).

</TabItem>
</Tabs>

### The `useQuery` Hook

Wasp's `useQuery` hook is a thin wrapper around the `useQuery` hook from [_react-query_](https://github.com/tannerlinsley/react-query).
One key difference is that Wasp doesn't expect you to supply the cache key - it takes care of it under the hood.

Wasp's `useQuery` hook accepts three arguments:

- `queryFn` <Required />

  The client-side query function generated by Wasp based on a `query` declaration in your `.wasp` file.

- `queryFnArgs`

  The arguments object (payload) you wish to pass into the Query. The Query's NodeJS implementation will receive this object as its first positional argument.

- `options`

  A _react-query_ `options` object. Use this to change
  [the default
  behavior](https://react-query.tanstack.com/guides/important-defaults) for
  this particular Query. If you want to change the global defaults, you can do
  so in the [client setup function](/docs/project/client-config.md#overriding-default-behaviour-for-queries).

For an example of usage, check [this section](#the-usequery-hook).
