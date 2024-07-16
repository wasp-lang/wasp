---
title: 5. Querying the Database
---

import useBaseUrl from '@docusaurus/useBaseUrl';
import { ShowForTs, ShowForJs } from '@site/src/components/TsJsHelpers';

We want to know which tasks we need to do, so let's list them!

The primary way of working with Entities in Wasp is with [Queries and Actions](../data-model/operations/overview), collectively known as **_Operations_**.

Queries are used to read an entity, while Actions are used to create, modify, and delete entities. Since we want to list the tasks, we'll want to use a Query.

To list the tasks, you must:

1. Create a Query that fetches the tasks from the database.
2. Update the `MainPage.{jsx,tsx}` to use that Query and display the results.

## Defining the Query

We'll create a new Query called `getTasks`. We'll need to declare the Query in the Wasp file and write its implementation in <ShowForJs>JS</ShowForJs><ShowForTs>TS</ShowForTs>.

### Declaring a Query

We need to add a **query** declaration to `main.wasp` so that Wasp knows it exists:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...

query getTasks {
  // Specifies where the implementation for the query function is.
  // The path `@src/queries` resolves to `src/queries.js`.
  // No need to specify an extension.
  fn: import { getTasks } from "@src/queries",
  // Tell Wasp that this query reads from the `Task` entity. Wasp will
  // automatically update the results of this query when tasks are modified.
  entities: [Task]
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

query getTasks {
  // Specifies where the implementation for the query function is.
  // The path `@src/queries` resolves to `src/queries.ts`.
  // No need to specify an extension.
  fn: import { getTasks } from "@src/queries",
  // Tell Wasp that this query reads from the `Task` entity. Wasp will
  // automatically update the results of this query when tasks are modified.
  entities: [Task]
}
```

</TabItem>
</Tabs>

### Implementing a Query

<ShowForJs>

Next, create a new file called `src/queries.js` and define the JavaScript function we've just imported in our `query` declaration:

</ShowForJs>
<ShowForTs>

Next, create a new file called `src/queries.ts` and define the TypeScript function we've just imported in our `query` declaration:

</ShowForTs>

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/queries.js"
export const getTasks = async (args, context) => {
  return context.entities.Task.findMany({
    orderBy: { id: 'asc' },
  })
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```js title="src/queries.ts"
import { Task } from 'wasp/entities'
import { type GetTasks } from 'wasp/server/operations'

export const getTasks: GetTasks<void, Task[]> = async (args, context) => {
  return context.entities.Task.findMany({
    orderBy: { id: 'asc' },
  })
}
```

Wasp automatically generates the types `GetTasks` and `Task` based on the contents of `main.wasp`:

- `Task` is a type corresponding to the `Task` entity you defined in `schema.prisma`.
- `GetTasks` is a generic type Wasp automatically generated based on the `getTasks` Query you defined in `main.wasp`.

You can use these types to specify the Query's input and output types. This Query doesn't expect any arguments (its input type is `void`), but it does return an array of tasks (its output type is `Task[]`).

Annotating the Queries is optional, but highly recommended because doing so enables **full-stack type safety**. We'll see what this means in the next step.

</TabItem>
</Tabs>

Query function parameters:
 - `args: object` 

  The arguments the caller passes to the Query.
- `context`

  An object with extra information injected by Wasp. Its type depends on the Query declaration.

Since the Query declaration in `main.wasp` says that the `getTasks` Query uses `Task` entity, Wasp injected a [Prisma client](https://www.prisma.io/docs/reference/tools-and-interfaces/prisma-client/crud) for the `Task` entity as `context.entities.Task` - we used it above to fetch all the tasks from the database.

:::info
Queries and Actions are NodeJS functions executed on the server.
:::

## Invoking the Query On the Frontend

While we implement Queries on the server, Wasp generates client-side functions that automatically take care of serialization, network calls, and cache invalidation, allowing you to call the server code like it's a regular function.

This makes it easy for us to use the `getTasks` Query we just created in our React component:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx {1,4-13,16-35} title="src/MainPage.jsx"
import { getTasks, useQuery } from 'wasp/client/operations'

export const MainPage = () => {
  const { data: tasks, isLoading, error } = useQuery(getTasks)

  return (
    <div>
      {tasks && <TasksList tasks={tasks} />}

      {isLoading && 'Loading...'}
      {error && 'Error: ' + error}
    </div>
  )
}

const TaskView = ({ task }) => {
  return (
    <div>
      <input type="checkbox" id={String(task.id)} checked={task.isDone} />
      {task.description}
    </div>
  )
}

const TasksList = ({ tasks }) => {
  if (!tasks?.length) return <div>No tasks</div>

  return (
    <div>
      {tasks.map((task, idx) => (
        <TaskView task={task} key={idx} />
      ))}
    </div>
  )
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx {1-2,5-14,17-36} title="src/MainPage.tsx"
import { Task } from 'wasp/entities'
import { getTasks, useQuery } from 'wasp/client/operations'

export const MainPage = () => {
  const { data: tasks, isLoading, error } = useQuery(getTasks)

  return (
    <div>
      {tasks && <TasksList tasks={tasks} />}

      {isLoading && 'Loading...'}
      {error && 'Error: ' + error}
    </div>
  )
}

const TaskView = ({ task }: { task: Task }) => {
  return (
    <div>
      <input type="checkbox" id={String(task.id)} checked={task.isDone} />
      {task.description}
    </div>
  )
}

const TasksList = ({ tasks }: { tasks: Task[] }) => {
  if (!tasks?.length) return <div>No tasks</div>

  return (
    <div>
      {tasks.map((task, idx) => (
        <TaskView task={task} key={idx} />
      ))}
    </div>
  )
}
```

</TabItem>
</Tabs>

Most of this code is regular React, the only exception being the <ShowForJs>two</ShowForJs><ShowForTs>three</ShowForTs> special `wasp` imports:

<ShowForJs>

- `getTasks` - The client-side Query function Wasp generated based on the `getTasks` declaration in `main.wasp`.
- `useQuery` - Wasp's [useQuery](../data-model/operations/queries#the-usequery-hook-1) React hook, which is based on [react-query](https://github.com/tannerlinsley/react-query)'s hook with the same name.

</ShowForJs>

<ShowForTs>

- `getTasks` - The client-side Query function Wasp generated based on the `getTasks` declaration in `main.wasp`.
- `useQuery` - Wasp's [useQuery](../data-model/operations/queries#the-usequery-hook-1) React hook, which is based on [react-query](https://github.com/tannerlinsley/react-query)'s hook with the same name.
- `Task` - The type for the Task entity defined in `schema.prisma`.

Notice how you don't need to annotate the type of the Query's return value: Wasp uses the types you defined while implementing the Query for the generated client-side function. This is **full-stack type safety**: the types on the client always match the types on the server.

</ShowForTs>

We could have called the Query directly using `getTasks()`, but the `useQuery` hook makes it reactive: React will re-render the component every time the Query changes. Remember that Wasp automatically refreshes Queries whenever the data is modified.

With these changes, you should be seeing the text "No tasks" on the screen:

<img alt="Todo App - No Tasks"
src={useBaseUrl('img/todo-app-no-tasks.png')}
style={{ border: "1px solid black" }}
/>

We'll create a form to add tasks in the next step 🪄
