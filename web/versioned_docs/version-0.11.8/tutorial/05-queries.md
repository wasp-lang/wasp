---
title: 5. Querying the Database
---

import useBaseUrl from '@docusaurus/useBaseUrl';
import { ShowForTs, ShowForJs } from '@site/src/components/TsJsHelpers';

We want to know which tasks we need to do, so let's list them! The primary way of interacting with entities in Wasp is by using [queries and actions](../data-model/operations/overview), collectively known as _operations_.

Queries are used to read an entity, while actions are used to create, modify, and delete entities. Since we want to list the tasks, we'll want to use a query.

To list tasks we have to:

1. Create a query that fetches tasks from the database.
2. Update the `MainPage.{jsx,tsx}` to use that query and display the results.

## Defining the Query

We'll create a new query called `getTasks`. We'll need to declare the query in the Wasp file and write its implementation in <ShowForJs>JS</ShowForJs><ShowForTs>TS</ShowForTs>.

### Declaring a Query

We need to add a **query** declaration to `main.wasp` so that Wasp knows it exists:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...

query getTasks {
  // Specifies where the implementation for the query function is.
  // Use `@server` to import files inside the `src/server` folder.
  fn: import { getTasks } from "@server/queries.js",
  // Tell Wasp that this query reads from the `Task` entity. By doing this, Wasp
  // will automatically update the results of this query when tasks are modified.
  entities: [Task]
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

query getTasks {
  // Specifies where the implementation for the query function is.
  // Use `@server` to import files inside the `src/server` folder.
  fn: import { getTasks } from "@server/queries.js",
  // Tell Wasp that this query reads from the `Task` entity. By doing this, Wasp
  // will automatically update the results of this query when tasks are modified.
  entities: [Task]
}
```

:::warning Importing Typescript files
Even though you are using TypeScript and plan to implement this query in `src/server/queries.ts`, you still need to import it using a `.js` extension. Wasp internally uses `esnext` module resolution, which requires importing all files with a `.js` extension. This is only needed when importing `@server@` files.

Read more about ES modules in TypeScript [here](https://www.typescriptlang.org/docs/handbook/esm-node.html). If you're interested in the discussion and the reasoning behind this, read about it [in this GitHub issue](https://github.com/microsoft/TypeScript/issues/33588).
:::

</TabItem>
</Tabs>

### Implementing a Query

<ShowForJs>

Next, create a new file `src/server/queries.js` and define the JavaScript function we've just imported in our `query` declaration:

</ShowForJs>
<ShowForTs>

Next, create a new file `src/server/queries.ts` and define the TypeScript function we've just imported in our `query` declaration:

</ShowForTs>

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/server/queries.js"
export const getTasks = async (args, context) => {
  return context.entities.Task.findMany({
    orderBy: { id: 'asc' },
  })
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```js title="src/server/queries.ts"
import { Task } from '@wasp/entities'
import { GetTasks } from '@wasp/queries/types'

export const getTasks: GetTasks<void, Task[]> = async (args, context) => {
  return context.entities.Task.findMany({
    orderBy: { id: 'asc' },
  })
}
```

Wasp automatically generates the types `GetTasks` and `Task` based the contents of `main.wasp`:

- `Task` is a type corresponding to the `Task` entity we've defined in `main.wasp`.
- `GetTasks` is a generic type Wasp automatically generated based the `getTasks` query we've defined in `main.wasp`.

You can use these types to specify the Query's input and output types. This query doesn't expect any arguments (its input type is `void`), but it does return an array of tasks (its output type is `Task[]`).

Annotating the queries is optional, but highly recommended because doing so enables **full-stack type safety**. We'll see what this means in the next step.

</TabItem>
</Tabs>

Query function parameters:

- `args`: `object`, arguments the query is given by the caller.
- `context`: `object`, information provided by Wasp.

Since we declared in `main.wasp` that our query uses the `Task` entity, Wasp injected a [Prisma client](https://www.prisma.io/docs/reference/tools-and-interfaces/prisma-client/crud) for the `Task` entity as `context.entities.Task` - we used it above to fetch all the tasks from the database.

:::info
Queries and actions are NodeJS functions that are executed on the server. Therefore, we put them in the `src/server` folder.
:::

## Invoking the Query On the Frontend

While we implement queries on the server, Wasp generates client-side functions that automatically takes care of serialization, network calls, and chache invalidation, allowing you to call the server code like it's a regular function. This makes it easy for us to use the `getTasks` query we just created in our React component:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx {1-2,5-14,17-36} title="src/client/MainPage.jsx"
import getTasks from '@wasp/queries/getTasks'
import { useQuery } from '@wasp/queries'

const MainPage = () => {
  const { data: tasks, isLoading, error } = useQuery(getTasks)

  return (
    <div>
      {tasks && <TasksList tasks={tasks} />}

      {isLoading && 'Loading...'}
      {error && 'Error: ' + error}
    </div>
  )
}

const Task = ({ task }) => {
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
        <Task task={task} key={idx} />
      ))}
    </div>
  )
}

export default MainPage
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx {1-3,6-15,18-37} title="src/client/MainPage.tsx"
import getTasks from '@wasp/queries/getTasks'
import { useQuery } from '@wasp/queries'
import { Task } from '@wasp/entities'

const MainPage = () => {
  const { data: tasks, isLoading, error } = useQuery(getTasks)

  return (
    <div>
      {tasks && <TasksList tasks={tasks} />}

      {isLoading && 'Loading...'}
      {error && 'Error: ' + error}
    </div>
  )
}

const Task = ({ task }: { task: Task }) => {
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
        <Task task={task} key={idx} />
      ))}
    </div>
  )
}

export default MainPage
```

</TabItem>
</Tabs>

Most of this code is regular React, the only exception being the <ShowForJs>two</ShowForJs><ShowForTs>three</ShowForTs> special `@wasp` imports:

<ShowForJs>

- `import getTasks from '@wasp/queries/getTasks'` - Imports the client-side query function.
- `import { useQuery } from '@wasp/queries'` - Imports Wasp's [useQuery](../data-model/operations/queries#the-usequery-hook-1) React hook, which is based on [react-query](https://github.com/tannerlinsley/react-query)'s hook with the same name.

</ShowForJs>

<ShowForTs>

- `import getTasks from '@wasp/queries/getTasks'` - Imports the client-side query function.
- `import { useQuery } from '@wasp/queries'` - Imports Wasp's [useQuery](../data-model/operations/queries#the-usequery-hook-1) React hook, which is based on [react-query](https://github.com/tannerlinsley/react-query)'s hook with the same name.
- `import { Task } from '@wasp/entities'` - The type for the task entity we defined in `main.wasp`.

Notice how you don't need to annotate the type of the query's return value: Wasp uses the types you defined while implementing the query for the generated client-side function. This is **full-stack type safety**: the types on the client always match the types on the server.

</ShowForTs>

We could have called the query directly using `getTasks()`, but the `useQuery` hook makes it reactive: React will re-render the component every time the query changes. Remember that Wasp automatically refreshes queries whenever the data is modified.

With these changes, you should be seeing the text "No tasks" on the screen:

<img alt="Todo App - No Tasks"
src={useBaseUrl('img/todo-app-no-tasks.png')}
style={{ border: "1px solid black" }}
/>

We'll create a form to add tasks in the next step 🪄
