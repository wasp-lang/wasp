---
title: 5. Querying the Database
hide_table_of_contents: true
---

import useBaseUrl from '@docusaurus/useBaseUrl';
import { TutorialAction } from './TutorialAction';
import { Scrollycoding } from '@site/src/components/Scrollycoding';

We want to know which tasks we need to do, so let's list them!

The primary way of working with Entities in Wasp is with [Queries and Actions](../data-model/operations/overview), collectively known as **_Operations_**.

Queries are used to read an entity, while Actions are used to create, modify, and delete entities. Since we want to list the tasks, we'll want to use a Query.

To list the tasks, you must:

1. Create a Query that fetches the tasks from the database.
2. Update the `MainPage.{jsx,tsx}` to use that Query and display the results.

## Creating and Using the Query

<Scrollycoding>

## !!steps Declaring the Query

We'll create a new Query called `getTasks`. First, we add a **query** specification to `main.wasp.ts` so that Wasp knows it exists.

We tell Wasp that this Query reads from the `Task` entity. Wasp will automatically update the Query's results whenever tasks are modified.

<TutorialAction id="query-get-tasks" action="APPLY_PATCH" />

:::note
To generate the types used in the next step, make sure that `wasp start` is still running.
:::

```ts ! main.wasp.ts
// !mark
import { app, page, query, route } from "@wasp.sh/spec";
import { MainPage } from "./src/MainPage" with { type: "ref" };
// !mark
import { getTasks } from "./src/queries" with { type: "ref" };

export default app({
  name: "TodoApp",
  wasp: { version: "{latestWaspVersion}" },
  title: "TodoApp",
  head: ["<link rel='icon' href='/favicon.ico' />"],
  spec: [
    route("RootRoute", "/", page(MainPage)),
    // !mark
    query(getTasks, { entities: ["Task"] }),
  ],
});
```

## !!steps Implementing the Query

Next, create a new file called `src/queries.ts` and define the TypeScript function we've just imported in our `query` spec.

Wasp automatically generates the types `GetTasks` and `Task` based on the contents of `main.wasp.ts`:

- `Task` corresponds to the `Task` entity you defined in `schema.prisma`.
- `GetTasks` is a generic type Wasp generated based on the `getTasks` Query you declared.

This Query doesn't expect any arguments (its input type is `void`), but it returns an array of tasks (its output type is `Task[]`). Annotating the Query is optional but highly recommended, because doing so enables **full-stack type safety**.

The Query function receives two parameters:

- `args`: the arguments the caller passes to the Query.
- `context`: an object with extra information injected by Wasp. Since the spec says `getTasks` uses the `Task` entity, Wasp injects a [Prisma client](https://www.prisma.io/docs/reference/tools-and-interfaces/prisma-client/crud) for it as `context.entities.Task`, which we use above to fetch all the tasks.

:::info
Queries and Actions are NodeJS functions executed on the server.
:::

<TutorialAction id="query-get-tasks-impl" action="APPLY_PATCH" />

```ts ! src/queries.ts
import type { Task } from "wasp/entities";
import type { GetTasks } from "wasp/server/operations";

export const getTasks: GetTasks<void, Task[]> = async (args, context) => {
  return context.entities.Task.findMany({
    orderBy: { id: "asc" },
  });
};
```

## !!steps Using the Query in React

While we implement Queries on the server, Wasp generates client-side functions that take care of serialization, network calls, and cache invalidation, so you can call the server code like a regular function.

Most of this is regular React. The only special parts are the three `wasp` imports:

- `getTasks`: the client-side Query function Wasp generated from the `getTasks` spec.
- `useQuery`: Wasp's [useQuery](../data-model/operations/queries#the-usequery-hook-1) React hook, based on [react-query](https://github.com/tannerlinsley/react-query)'s hook of the same name.
- `Task`: the type for the Task entity defined in `schema.prisma`.

Notice you don't need to annotate the Query's return value: Wasp reuses the types from the Query's implementation for the generated client-side function. This is **full-stack type safety**: the types on the client always match the types on the server.

We could have called the Query directly with `getTasks()`, but the `useQuery` hook makes it reactive: React re-renders the component every time the Query's result changes.

<TutorialAction id="main-page-tasks" action="APPLY_PATCH" />

```tsx ! src/MainPage.tsx
import type { Task } from "wasp/entities";
// !mark
import { getTasks, useQuery } from "wasp/client/operations";

export const MainPage = () => {
  // !mark
  const { data: tasks, isLoading, error } = useQuery(getTasks);

  return (
    <div>
      {tasks && <TasksList tasks={tasks} />}

      {isLoading && "Loading..."}
      {error && "Error: " + error}
    </div>
  );
};

const TaskView = ({ task }: { task: Task }) => {
  return (
    <div>
      <input type="checkbox" id={String(task.id)} checked={task.isDone} />
      {task.description}
    </div>
  );
};

const TasksList = ({ tasks }: { tasks: Task[] }) => {
  if (!tasks?.length) return <div>No tasks</div>;

  return (
    <div>
      {tasks.map((task, idx) => (
        <TaskView task={task} key={idx} />
      ))}
    </div>
  );
};
```

</Scrollycoding>

With these changes, you should be seeing the text "No tasks" on the screen:

<img alt="Todo App - No Tasks" src={useBaseUrl('img/todo-app-no-tasks.png')} className="tutorial-image" />

We'll create a form to add tasks in the next step 🪄
