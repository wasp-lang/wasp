---
id: 03-listing-tasks
title: "Listing tasks"
---

import useBaseUrl from '@docusaurus/useBaseUrl';
import { ShowForTs, ShowForJs } from '@site/src/components/TsJsHelpers';

We want to admire our tasks, so let's list them!

## Introducing operations (queries and actions)

The primary way of interacting with entities in Wasp is via [operations (queries and actions)](language/features.md#queries-and-actions-aka-operations).

Queries are here when we need to fetch/read something, while actions are here when we need to change/update something.
We will start with writing a query, since we are just listing tasks and not modifying anything for now.

To list tasks, we will need two things:
1. A Wasp query that fetches all the tasks from the database.
2. React logic that calls our query and displays its results.

## Defining the Query

Let's implement `getTasks` [query](language/features.md#query).

<ShowForJs>

It consists of a declaration in Wasp and implementation in JS (in `src/server/` directory).
</ShowForJs>
<ShowForTs>

It consists of a declaration in Wasp and implementation in TS (in `src/server/` directory).
</ShowForTs>

### Wasp declaration
Add the following code to `main.wasp`:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...

query getTasks {
  // We specify that JS implementation of the query (which is an async JS function)
  // can be found in `src/server/queries.js` as the named export `getTasks`.
  // Use '@server' to reference files inside the src/server folder.
  fn: import { getTasks } from "@server/queries.js",
  // We tell Wasp that this query is doing something with entity `Task`. With that, Wasp will
  // automatically refresh the results of this query when tasks change.
  entities: [Task]
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

query getTasks {
  // We specify that JS implementation of the query (which is an async JS function)
  // can be found in `src/server/queries.js` as the named export `getTasks`.
  // Use '@server' to reference files inside the src/server folder.
  fn: import { getTasks } from "@server/queries.js",
  // We tell Wasp that this query is doing something with entity `Task`. With that, Wasp will
  // automatically refresh the results of this query when tasks change.
  entities: [Task]
}
```

:::caution
<!-- This block is mostly duplicated in typescript.md -->
Even if you use TypeScript and have the file `queries.ts`, you will still need to import it using the `.js` extension. Wasp internally uses `esnext` module resolution, which always requires specifying the extension as `.js` (i.e., the extension used in the emitted JS file). This applies to all `@server` imports (and files on the server in general). It does not apply to client files.

Read more about ES modules in TypeScript [here](https://www.typescriptlang.org/docs/handbook/esm-node.html). If you're interested in the discussion and the reasoning behind this, read about it [in this GitHub issue](https://github.com/microsoft/TypeScript/issues/33588).
:::

</TabItem>
</Tabs>

<ShowForJs>

### JavaScript implementation

Next, create a new file `src/server/queries.ts` and define the JavaScript function we've just imported in our `query` declaration:
</ShowForJs>
<ShowForTs>

### TypeScript implementation

Next, create a new file `src/server/queries.ts` and define the TypeScript function we've just imported in our `query` declaration:
</ShowForTs>



<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/server/queries.js"
export const getTasks = async (args, context) => {
  return context.entities.Task.findMany({})
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```js title="src/server/queries.ts"
import { Task }  from "@wasp/entities"
import { GetTasks } from "@wasp/queries/types"

export const getTasks: GetTasks<void, Task[]> = async (args, context) => {
  return context.entities.Task.findMany({})
}
```

Wasp automatically generates the types `GetTasks` and `Task` based the contents of `main.wasp`:
  - `Task` is a type corresponding to the `Task` entity we've defined in `main.wasp`.
  - `GetTasks` is a generic type Wasp automatically generated based the `getTasks` query we've defined in `main.wasp`.

You can use these types to specify the Query's input and output types. This Query doesn't expect any arguments (meaning that its input type is `void`), but it does return an array of tasks (meaning that its output type is `Task[]`)

Annotating the Queries is optional, but highly recommended because doing so enables **full-stack type safety**. We'll see what this means in the next section.

</TabItem>
</Tabs>

Query function parameters:
- `args`: `object`, arguments the query is invoked with.
- `context`: `object`, additional stuff provided by Wasp.


Since we declared in `main.wasp` that our query uses the `Task` entity, Wasp injected a [Prisma client](https://www.prisma.io/docs/reference/tools-and-interfaces/prisma-client/crud) for the `Task` entity as `context.entities.Task` - we used it above to fetch all the tasks from the database.

:::info
Queries and actions are NodeJS functions that are executed on the server. Therefore, we put them in the `src/server` folder.
:::

## Invoking the Query on the frontend

We've just said that the queries we write are executed on the server, but Wasp will generate client-side query functions (taking care of serialization, network calls, and cache invalidation in the background). Let's finally use the query we've just created, `getTasks`, in our React component to list the tasks:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx {1-2,5-14,17-36} title="src/client/MainPage.jsx"
import getTasks from "@wasp/queries/getTasks"
import { useQuery } from "@wasp/queries"

const MainPage = () => {
  const { data: tasks, isLoading, error } = useQuery(getTasks)

  return (
    <div>
      {tasks && <TasksList tasks={tasks} />}

      {isLoading && "Loading..."}
      {error && "Error: " + error}
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
import getTasks from "@wasp/queries/getTasks"
import { useQuery } from "@wasp/queries"
import { Task } from "@wasp/entities"

const MainPage = () => {
  const { data: tasks, isLoading, error } = useQuery(getTasks)

  return (
    <div>
      {tasks && <TasksList tasks={tasks} />}

      {isLoading && "Loading..."}
      {error && "Error: " + error}
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

<ShowForJs>

Most of this code is just regular React, the only exception being the two special `@wasp` imports:
 - `import getTasks from '@wasp/queries/getTasks'` - Gives us our freshly defined Wasp query.
 - `import { useQuery } from '@wasp/queries'` - Gives us Wasp's [useQuery](language/features.md#the-usequery-hook) React hook which is actually just a thin wrapper over [react-query](https://github.com/tannerlinsley/react-query)'s [useQuery](https://react-query.tanstack.com/docs/guides/queries) hook, behaving very similarly while offering some extra integration with Wasp.

</ShowForJs>

<ShowForTs>

Most of this code is just regular React, the only exception being the three special `@wasp` imports:
 - `import getTasks from '@wasp/queries/getTasks'` - Gives us our freshly defined Wasp query.
 - `import { useQuery } from '@wasp/queries'` - Gives us Wasp's [useQuery](language/features.md#the-usequery-hook) React hook which is actually just a thin wrapper over [react-query](https://github.com/tannerlinsley/react-query)'s [useQuery](https://react-query.tanstack.com/docs/guides/queries) hook, behaving very similarly while offering some extra integration with Wasp.
 - `import { Task } from '@wasp/entities'` - The type for Task entity we've defined in `main.wasp`.

Notice how you didn't need to tell TypeScript anything about the Query's response data - TypeScript inferred it automatically.

Because we've previously annotated the Query's backend implementation with `GetTasks<void, Task[]>`, Wasp knows the response data's correct type (i.e., `Task[]`) on the frontend. We call this feature **full-stack type safety**.
 
</ShowForTs>

We could have called the Query directly with `getTasks()`, but wrapping it with `useQuery(getTasks)` makes it reactive. More precisely, React will re-render the component every time the Query's result changes.

With these changes, you should be seeing the text "No tasks" on the screen:

<img alt="Todo App - No Tasks"
     src={useBaseUrl('img/todo-app-no-tasks.png')}
     style={{ border: "1px solid black" }}
/>

Next, let's create some tasks!
