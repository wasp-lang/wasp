---
title: Operations (Queries & Actions)
---

import { Required } from '@site/src/components/Required';

While Entities enable help you define your app's data model and relationships, Operations are all about working with this data.

There are two kinds of Operations: **Queries** and **Actions**. As their names suggest,
Queries are meant for reading data, and Actions are meant for changing it (either by updating existing entries or creating new ones).

### Queries

You can use Queries to fetch data from the server. They shouldn't modify the server's state.

You implement Queries in NodeJS, and they are executed within the server's context.
Wasp generates the code that lets you call the Query from anywhere in your code (client or server) via the same interface.
In other words, you don't have to worry about building an HTTP API for the Query, handling the request on the server, or even handling and caching the responses on the client.
Instead, simply focus on the business logic inside your Query and let Wasp take care of the rest!

To create a Wasp Query, you must:

1. Declare the Query in Wasp using the `query` declaration.
2. Define the Query's NodeJS implementation.

After completing these two steps, you'll be able to use the Query from any point in your code.

#### Declaring a Query in Wasp

We'll start by telling Wasp we wish to create a Query.

You can easily do this with the `query` declaration, which supports the following fields:

- `fn: ServerImport` <Required /> - The import statement of the Query's NodeJs implementation.
- `entities: [Entity]` - A list of entities you wish to use inside your Query.
  We'll leave this option aside for now. We'll go deeper into it [in a bit](#using-entities-in-queries).

Wasp Queries and their implementations don't need to (but can) have the same name. We'll keep the names identical to avoid confusion.
With that in mind, here's how you might declare two queries, one for fetching all tasks and another for fetching filtered tasks:

```wasp title="main.wasp"
// ...

query getAllTasks {
  fn: import { getAllTasks } from "@server/queries.js"
}

query getFilteredTasks {
  fn: import { getFilteredTasks } from "@server/queries.js"
}
```

You might have noticed that we've told Wasp to import Query implementations that don't yet exist. Don't worry about that for now. We'll implement them in the next section. It's a good idea to start with the high-level concept (i.e., the Query declaration in the Wasp file) and only then deal with the implementation details (i.e., the Query's implementation in JavaScript).

After declaring a Wasp Query, two crucial things happen:

- Wasp **generates a client-side JavaScript function** that shares its name with the Query (e.g., `getFilteredTasks`).
  This function takes a single optional argument - an object containing any serializable data you wish to use inside the Query.
  Wasp will pass this object to the Query's implementation as its first positional argument (i.e., `args` from the previous step).
  Such an abstraction works thanks to an HTTP API route handler Wasp generates on the server, which calls the Query's NodeJS implementation under the hood.
- Wasp **generates a server-side NodeJS function** that shares its name with the Query. This function's interface is identical to the client-side function from the previous point.

Generating two such functions ensures a uniform calling interface across the entire app (both client and server).

#### Defining the Query's NodeJS implementation

Now that we've declared the Query, all that's left is implementing it.
The Query's implementation is a NodeJS function that takes two arguments (it can be an `async` function if you need to use the `await` keyword).
Since both arguments are positional, you can name the parameters however you want, but we'll stick with `args` and `context`:

1. `args`

An object containing all the arguments (i.e., payload) **passed to the Query by the caller** (e.g., filtering conditions).
Look at [the examples of usage](#using-the-query) to see how to pass this object to the Query. 2. `context`

An additional context object **injected into the Query by Wasp**. This object contains user session information, as well as information about entities. The examples here won't use the context for simplicity purposes. You can read more about it in the [section about using entities in Queries](#using-entities-in-queries).

We've told Wasp to look for Queries' implementations in the file `src/server/queries.{js,ts}`, so let's create the file and write the implementations!

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/server/queries.js"
// our "database"
const tasks = [
  { id: 1, description: "Buy some eggs", isDone: true },
  { id: 2, description: "Make an omelette", isDone: false },
  { id: 3, description: "Eat breakfast", isDone: false },
];

// You don't need to use the arguments if you don't need them
export const getAllTasks = () => {
  return tasks;
};

// The 'args' object is something sent by the caller (most often from the client)
export const getFilteredTasks = (args) => {
  const { isDone } = args;
  return tasks.filter((task) => task.isDone === isDone);
};
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/server/queries.ts"
import { GetAllTasks, GetFilteredTasks } from "@wasp/queries/types";

type Task = {
  id: number;
  description: string;
  isDone: boolean;
};

// our "database"
const tasks: Task[] = [
  { id: 1, description: "Buy some eggs", isDone: true },
  { id: 2, description: "Make an omelette", isDone: false },
  { id: 3, description: "Eat breakfast", isDone: false },
];

// You don't need to use the arguments if you don't need them
export const getAllTasks: GetAllTasks<void, Task[]> = () => {
  return tasks;
};

// The 'args' object is something sent by the caller (most often from the client)
export const getFilteredTasks: GetFilteredTasks<
  Pick<Task, "isDone">,
  Task[]
> = (args) => {
  const { isDone } = args;
  return tasks.filter((task) => task.isDone === isDone);
};
```

Wasp automatically generates the types `GetTasks` and `GetFilteredTasks` based on the declarations in your Wasp file:

- `GetTasks` is a generic type Wasp automatically generated based on the Query `getTasks`.
- `GetFilteredTasks` is a generic type Wasp automatically generated based on the Query `getFilteredTasks`.

You can use these types to specify the Query's input and output types. The Query `getTasks` doesn't expect any arguments (its input type is `void`), but it does return an array of tasks (its output type is `Task[]`).

The Query `getFilteredTasks`, on the other hand, expects an object of type `{ isDone: boolean }`. We've derived this type from the type `Task`.

Annotating the Queries is optional, but highly recommended. Doing so enables **full-stack type safety**. We'll see what this means when using the Query from the client.

</TabItem>
</Tabs>

#### Using the Query

To use the Query, you can import it from `@wasp` and call it directly. As mentioned, the usage doesn't change depending on whether you're on the server or the client:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```javascript
import getAllTasks from "@wasp/queries/getAllTasks.js";
import getFilteredTasks from "@wasp/queries/getFilteredTasks.js";

// ...

const allTasks = await getAllTasks();
const doneTasks = await getFilteredTasks({ isDone: true });
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```typescript
import getAllTasks from "@wasp/queries/getAllTasks.js";
import getFilteredTasks from "@wasp/queries/getFilteredTasks.js";

// TypeScript automatically infers the return values and type-checks
// the payloads.
const allTasks = await getAllTasks();
const doneTasks = await getFilteredTasks({ isDone: true });
```

</TabItem>
</Tabs>

**NOTE**: Wasp will not stop you from importing a Query's NodeJS implementation from `./queries.js` and calling it directly. However, try to avoid doing this. Besides losing all useful features a Wasp Query provides (e.g., entity injection), the raw Query will expect you to provide a context as a second argument (which is cumbersome).

#### The `useQuery` hook

When using Queries on the client, you can make them reactive with the `useQuery` hook.
This hook comes bundled with Wasp and is a thin wrapper around the `useQuery` hook from [_react-query_](https://github.com/tannerlinsley/react-query).

Wasp's `useQuery` hook accepts three arguments:

- `queryFn` <Required />

  A Wasp Query declared in the previous step or, in other words, the client-side query function generated by Wasp based on a `query` declaration in your `.wasp` file.

- `queryFnArgs`

  The arguments object (payload) you wish to pass into the Query. The Query's NodeJS implementation will receive this object as its first positional argument.

- `options`

  A _react-query_ `options` object. Use this to change
  [the default
  behavior](https://react-query.tanstack.com/guides/important-defaults) for
  this particular Query. If you want to change the global defaults, you can do
  so in the [client setup function](#overriding-default-behaviour-for-queries).

Wasp's `useQuery` hook behaves mostly the same as [_react-query_'s `useQuery` hook](https://react-query.tanstack.com/docs/api#usequery), the only difference being in not having to supply the key (Wasp does this automatically under the hood).

Here's an example of calling the Queries using the `useQuery` hook:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx
import React from "react";
import { useQuery } from "@wasp/queries";
import getAllTasks from "@wasp/queries/getAllTasks";
import getFilteredTasks from "@wasp/queries/getFilteredTasks";

const MainPage = () => {
  const { data: allTasks, error: error1 } = useQuery(getAllTasks);
  const { data: doneTasks, error: error2 } = useQuery(getFilteredTasks, {
    isDone: true,
  });

  return (
    <div>
      <h2>All Tasks</h2>
      {allTasks
        ? allTasks.map((task) => <Task key={task.id} {...task} />)
        : error1}

      <h2>Finished Tasks</h2>
      {doneTasks
        ? doneTasks.map((task) => <Task key={task.id} {...task} />)
        : error2}
    </div>
  );
};

const Task = ({ description, isDone }) => {
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
    </div>
  );
};

export default MainPage;
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx
import React from "react";
import { Task } from "@wasp/entities";
import { useQuery } from "@wasp/queries";
import getAllTasks from "@wasp/queries/getAllTasks";
import getFilteredTasks from "@wasp/queries/getFilteredTasks";

const MainPage = () => {
  // Wasp will automatically infer and type-check payload types.
  const { data: allTasks, error: error1 } = useQuery(getAllTasks);
  const { data: doneTasks, error: error2 } = useQuery(getFilteredTasks, {
    isDone: true,
  });

  return (
    <div>
      <h2>All Tasks</h2>
      {allTasks
        ? allTasks.map((task) => <Task key={task.id} {...task} />)
        : error1}

      <h2>Finished Tasks</h2>
      {doneTasks
        ? doneTasks.map((task) => <Task key={task.id} {...task} />)
        : error2}
    </div>
  );
};

const Task = ({ description, isDone }: Task) => {
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
    </div>
  );
};

export default MainPage;
```

Notice how you don't need to annotate the Query's return value type: Wasp automatically infers the from the Query's backend implementation. This is **full-stack type safety**: the types on the client always match the types on the server.
</TabItem>
</Tabs>

#### Error Handling

For security reasons, all exceptions thrown in the Query's NodeJS implementation are sent to the client as responses with the HTTP status code `500`, with all other details removed.
Hiding error details by default helps against accidentally leaking possibly sensitive information over the network.

If you do want to pass additional error information to the client, you can construct and throw an appropriate `HttpError` in your NodeJS Query function:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title=src/server/queries.js
import HttpError from "@wasp/core/HttpError.js";

export const getAllTasks = async (args, context) => {
  const statusCode = 403;
  const message = "You can't do this!";
  const data = { foo: "bar" };
  throw new HttpError(statusCode, message, data);
};
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title=src/server/queries.ts
import { GetAllTasks } from "@wasp/types/queries";
import HttpError from "@wasp/core/HttpError.js";

export const getAllTasks: GetAllTasks = async (args, context) => {
  const statusCode = 403;
  const message = "You can't do this!";
  const data = { foo: "bar" };
  throw new HttpError(statusCode, message, data);
};
```

</TabItem>
</Tabs>

If the status code is `4xx`, the client will receive a response object with the corresponding `.message` and `.data` fields and rethrow the error (with these fields included).
To prevent information leakage, the server won't forward these fields for any other HTTP status codes.

#### Using Entities in Queries

In most cases, resources used in Queries will be [Entities](#entity).
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

:::warning
Even though you are using TypeScript and plan to implement this Query in `src/server/queries.ts`, you still need to import it using a `.js` extension. Wasp internally uses `esnext` module resolution, which requires importing all files with a `.js` extension. This is only needed when importing `@server@` files.

Read more about ES modules in TypeScript [here](https://www.typescriptlang.org/docs/handbook/esm-node.html). If you're interested in the discussion and the reasoning behind this, read about it [in this GitHub issue](https://github.com/microsoft/TypeScript/issues/33588).
:::
</TabItem>
</Tabs>

Wasp will inject the specified Entity into the Query's `context` argument, giving you access to the Entity's Prisma API:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/server/queries.js"
export const getAllTasks = async (args, context) => {
  return context.entities.Task.findMany({});
};

export const getFilteredTasks = async (args, context) => {
  return context.entities.Task.findMany({
    where: { isDone: args.isDone },
  });
};
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/server/queries.ts"
import { Task } from "@wasp/entities";
import { GetAllTasks, GetFilteredTasks } from "@wasp/queries/types";

export const getAllTasks: GetAllTasks<void, Task[]> = async (args, context) => {
  return context.entities.Task.findMany({});
};

export const getFilteredTasks: GetFilteredTasks<
  Pick<Task, "isDone">,
  Task[]
> = async (args, context) => {
  return context.entities.Task.findMany({
    where: { isDone: args.isDone },
  });
};
```

Again, annotating the Queries is optional, but greatly improves **full-stack type safety**.

</TabItem>
</Tabs>

The object `context.entities.Task` exposes `prisma.task` from [Prisma's CRUD API](https://www.prisma.io/docs/reference/tools-and-interfaces/prisma-client/crud).

### Action

Actions are very similar to Queries. So similar, in fact, we will only list the differences:

1. They can (and most often should) modify the server's state, while Queries are only allowed to read it.
2. Actions don't need to be reactive so you can call them directly. Still, Wasp does provide a `useAction` React hook for adding extra behavior to the Action (e.g., optimistic updates).
   Read more about the [`useAction` hook](#the-useaction-hook) below.
3. `action` declarations in Wasp are mostly identical to `query` declarations. The only difference is in the declaration's name.

Here's a declaration for an Action in Wasp:
<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...

action sayHi {
  fn: import { sayHi } from "@server/actions.js"
}
```

... Its corresponding implementation:

```js title=src/server/actions.js
export const sayHi = async () => {
  console.log("The client said Hi!");
};
```

And an example of how to import and call the declared Action:

```js
import sayHi from "@wasp/actions/sayHi";

// ...

sayHi();
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

action sayHi {
  fn: import { sayHi } from "@server/actions.js"
}
```

... Its corresponding implementation:

```js title=src/server/actions.ts
export const sayHi = async () => {
  console.log("The client said Hi!");
};
```

And an example of how to import and call the declared Action:

```ts
import sayHi from "@wasp/actions/sayHi";

// ...

sayHi();
```

</TabItem>
</Tabs>

Here's how you might define and use a less contrived Action.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title=main.wasp
action updateTaskIsDone {
  fn: import { updateTaskIsDone } from "@server/actions.js",
  entities: [Task]
}
```

```js title=src/server/actions.js
export const updateTaskIsDone = ({ id, isDone }, context) => {
  return context.entities.Task.update({
    where: { id },
    data: { isDone },
  });
};
```

```jsx {4,24} title=src/client/pages/Task.jsx
import React from "react";
import { useQuery } from "@wasp/queries";
import fetchTask from "@wasp/queries/fetchTask";
import updateTaskIsDone from "@wasp/actions/updateTaskIsDone";

const TaskPage = ({ id }) => {
  const { data: task } = useQuery(fetchTask, { id });

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
      <button onClick={() => updateTaskIsDone({ id, isDone: !isDone })}>
        Mark as {task.isDone ? "undone" : "done"}
      </button>
    </div>
  );
};
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title=main.wasp
action updateTaskIsDone {
  fn: import { updateTaskIsDone } from "@server/actions.js",
  entities: [Task]
}
```

```js title=src/server/actions.ts
import { Task } from "@wasp/entities";
import { UpdateTaskIsDone } from "@wasp/actions/types";

export const updateTaskIsDone: UpdateTaskIsDone<Pick<Task, "id" | "isDone">> = (
  { id, isDone },
  context
) => {
  return context.entities.Task.update({
    where: { id },
    data: { isDone },
  });
};
```

```jsx {4,24} title=src/client/pages/Task.tsx
import React from "react";
import { useQuery } from "@wasp/queries";
import getTask from "@wasp/queries/getTask";
import updateTaskIsDone from "@wasp/actions/updateTaskIsDone";

const TaskPage = ({ id }: { id: number }) => {
  const { data: task } = useQuery(getTask, { id });

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
      <button onClick={() => updateTaskIsDone({ id, isDone: !isDone })}>
        Mark as {task.isDone ? "undone" : "done"}
      </button>
    </div>
  );
};
```

</TabItem>
</Tabs>

#### The `useAction` hook

When using Actions in components, you can enhance them with the help of the `useAction` hook. This hook comes bundled with Wasp, and you can use it to decorate Wasp Actions.
In other words, the hook returns a function whose API matches the original Action while also doing something extra under the hood (depending on how you configure it).

The `useAction` hook accepts two arguments:

- `actionFn` <Required />

  The Wasp Action (i.e., the client-side Query function generated by Wasp based on a Action declaration) you wish to enhance.

- `actionOptions`

  An object configuring the extra features you want to add to the given Action. While this argument is technically optional, there is no point in using the `useAction` hook without providing it (it would be the same as using the Action directly). The Action options object supports the following fields:

  - `optimisticUpdates`

    An array of objects where each object defines an [optimistic update](https://stackoverflow.com/a/33009713) to perform on the Query cache. To define an optimistic update, you must specify the following properties:

    - `getQuerySpecifier` <Required />

    A function returning the Query specifier (i.e., a value used to address the Query you want to update). A Query specifier is an array specifying the query function and arguments. For example, to optimistically update the Query used with `useQuery(fetchFilteredTasks, {isDone: true }]`, your `getQuerySpecifier` function would have to return the array `[fetchFilteredTasks, { isDone: true}]`. Wasp will forward the argument you pass into the decorated Action to this function (i.e., you can use the properties of the added/changed item to address the Query).

    - `updateQuery` <Required />

    The function used to perform the optimistic update. It should return the desired state of the cache. Wasp will call it with the following arguments:

    - `item` - The argument you pass into the decorated Action.
    - `oldData` - The currently cached value for the Query identified by the specifier.

:::caution
The `updateQuery` function must be a pure function. It must return the desired cache value identified by the `getQuerySpecifier` function and _must not_ perform any side effects. Also, make sure you only update the Query caches affected by your Action causing the optimistic update (Wasp cannot yet verify this). Finally, your implementation of the `updateQuery` function should work correctly regardless of the state of `oldData` (e.g., don't rely on array positioning). If you need to do something else during your optimistic update, you can directly use _react-query_'s lower-level API (read more about it [here](#advanced-usage)).
:::

Here's an example showing how to configure the Action from the previous example to perform an optimistic update:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx {3,9,10,11,12,13,14,15,16,34} title=src/client/pages/Task.jsx
import React from "react";
import { useQuery } from "@wasp/queries";
import { useAction } from "@wasp/actions";
import fetchTask from "@wasp/queries/fetchTask";
import updateTaskIsDone from "@wasp/actions/updateTaskIsDone";

const TaskPage = ({ id }) => {
  const { data: task } = useQuery(fetchTask, { id });
  const updateTaskIsDoneOptimistically = useAction(updateTaskIsDone, {
    optimisticUpdates: [
      {
        getQuerySpecifier: ({ id }) => [fetchTask, { id }],
        updateQuery: ({ isDone }, oldData) => ({ ...oldData, isDone }),
      },
    ],
  });

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
      <button
        onClick={() => updateTaskIsDoneOptimistically({ id, isDone: !isDone })}
      >
        Mark as {task.isDone ? "undone" : "done"}
      </button>
      <div>
        <Link to="/">Back to main page</Link>
      </div>
    </div>
  );
};

export default TaskPage;
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```jsx {2,4,8,12,13,14,15,16,17,18,19,36} title=src/client/pages/Task.js
import React from "react";
import { Task } from "@wasp/entities";
import { useQuery } from "@wasp/queries";
import { useAction, OptimisticUpdateDefinition } from "@wasp/actions";
import getTask from "@wasp/queries/getTask";
import updateTaskIsDone from "@wasp/actions/updateTaskIsDone";

type TaskPayload = Pick<Task, "id" | "isDone">;

const TaskPage = ({ id }: { id: number }) => {
  const { data: task } = useQuery(getTask, { id });
  const updateTaskIsDoneOptimistically = useAction(updateTaskIsDone, {
    optimisticUpdates: [
      {
        getQuerySpecifier: ({ id }) => [getTask, { id }],
        updateQuery: ({ isDone }, oldData) => ({ ...oldData, isDone }),
      } as OptimisticUpdateDefinition<TaskPayload, Task>,
    ],
  });

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
      <button onClick={() => updateTaskIsDone({ id, isDone: !isDone })}>
        Mark as {task.isDone ? "undone" : "done"}
      </button>
    </div>
  );
};

export default TaskPage;
```

</TabItem>
</Tabs>

#### Advanced usage

The `useAction` hook currently only supports specifying optimistic updates. You can expect more features in future versions of Wasp.

Wasp's optimistic update API is deliberately small and focuses exclusively on updating Query caches (as that's the most common use case). You might need an API that offers more options or a higher level of control. If that's the case, instead of using Wasp's `useAction` hook, you can use _react-query_'s `useMutation` hook and directly work with [their low-level API](https://tanstack.com/query/v4/docs/guides/optimistic-updates?from=reactQueryV3&original=https://react-query-v3.tanstack.com/guides/optimistic-updates).

If you decide to use _react-query_'s API directly, you will need access to the Query's cache key. Wasp internally uses this key but abstracts it from the programmer. Still, you can easily obtain it by accessing the `queryCacheKey` property on a Query:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js
import { getTasks } from "@wasp/queries";

const queryKey = getTasks.queryCacheKey;
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts
import { getTasks } from "@wasp/queries";

const queryKey = getTasks.queryCacheKey;
```

</TabItem>
</Tabs>

### Cache Invalidation

One of the trickiest parts of managing a web app's state is making sure the data returned by the Queries is up to date.
Since Wasp uses _react-query_ for Query management, we must make sure to invalidate Queries (more specifically, their cached results managed by _react-query_) whenever they become stale.

It's possible to invalidate the caches manually through several mechanisms _react-query_ provides (e.g., refetch, direct invalidation).
However, since manual cache invalidation quickly becomes complex and error-prone, Wasp offers a quicker and a more effective solution to get you started: **automatic Entity-based Query cache invalidation**.
Because Actions can (and most often do) modify the state while Queries read it, Wasp invalidates a Query's cache whenever an Action that uses the same Entity is executed.

For example, let's assume that Action `createTask` and Query `getTasks` both use Entity `Task`. If `createTask` is executed, `getTasks`'s cached result may no longer be up-to-date.
Wasp will therefore invalidate it, making `getTasks` refetch data from the server, bringing it up to date again.

In practice, this means that Wasp keeps the Queries "fresh" without requiring you to think about cache invalidation.

On the other hand, this kind of automatic cache invalidation can become wasteful (some updates might not be necessary) and will only work for Entities. If that's an issue, you can use the mechanisms provided by _react-query_ for now, and expect more direct support in Wasp for handling those use cases in a nice, elegant way.

If you wish to optimistically set cache values after perfomring an Action, you can do so using [optimistic updates](https://stackoverflow.com/a/33009713). Configure them using Wasp's [useAction hook](#the-useaction-hook). This is currently the only manual cache invalidation mechanism Wasps supports natively. For everything else, you can always rely on _react-query_.

### Prisma Error Helpers

In your Operations, you may wish to handle general Prisma errors with HTTP-friendly responses. We have exposed two helper functions, `isPrismaError`, and `prismaErrorToHttpError`, for this purpose. As of now, we convert two specific Prisma errors (which we will continue to expand), with the rest being `500`. See the [source here](https://github.com/wasp-lang/wasp/blob/main/waspc/e2e-test/test-outputs/waspMigrate-golden/waspMigrate/.wasp/out/server/src/utils.js).

#### `import statement`:

```js
import { isPrismaError, prismaErrorToHttpError } from "@wasp/utils.js";
```

##### Example of usage:

```js
try {
  await context.entities.Task.create({...})
} catch (e) {
  if (isPrismaError(e)) {
    throw prismaErrorToHttpError(e)
  } else {
    throw e
  }
}
```
