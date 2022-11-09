---
title: "Listing tasks"
---

import useBaseUrl from '@docusaurus/useBaseUrl';

We want to admire our tasks, so let's list them!

## Introducing operations (queries and actions)

The primary way of interacting with entities in Wasp is via [operations (queries and actions)](language/features.md#queries-and-actions-aka-operations).

Queries are here when we need to fetch/read something, while actions are here when we need to change/update something.
We will start with writing a query, since we are just listing tasks and not modifying anything for now.

To list tasks, we will need two things:
1. Wasp query that fetches all the tasks from the database.
2. React logic that calls our query and displays its results.

## Wasp query

Let's implement `getTasks` [query](language/features.md#query).
It consists of a declaration in Wasp and implementation in JS (in `src/server/` directory).

### Wasp declaration
Add the following code to `main.wasp`:
```c title="main.wasp"
// ...

query getTasks {
  // We specify that JS implementation of the query (which is an async JS function)
  // can be found in `@server/queries.js` as the named export `getTasks`.
  fn: import { getTasks } from "@server/queries.js",
  // We tell Wasp that this query is doing something with entity `Task`. With that, Wasp will
  // automatically refresh the results of this query when tasks change.
  entities: [Task]
}
```

### JS implementation
Next, create a new file `src/server/queries.js` and define the JS we've just used in the `query` declaration above:

```js title="src/server/queries.js"
export const getTasks = async (args, context) => {
  return context.entities.Task.findMany({})
}
```

Query function parameters:
- `args`: `object`, arguments the query is invoked with.
- `context`: `object`, additional stuff provided by Wasp.


Since we declared in `main.wasp` that our query uses entity Task, Wasp injected a [Prisma client](https://www.prisma.io/docs/reference/tools-and-interfaces/prisma-client/crud) for entity Task as `context.entities.Task` - we used it above to fetch all the tasks from the database.

:::info
Queries and actions are NodeJS functions that are executed on the server. Therefore, we put them in the `src/server` folder.
:::

## Invoking the query from React

We've just said that the queries we write are executed on the server, but Wasp will generate client-side query functions (taking care of the network and cache invalidation in the background). Let's finally use the query we've just created, `getTasks`, in our React component to list the tasks:

```jsx {3-4,7-16,19-32} title="src/client/MainPage.js"
import React from 'react'

import getTasks from '@wasp/queries/getTasks'
import { useQuery } from '@wasp/queries'

const MainPage = () => {
  const { data: tasks, isFetching, error } = useQuery(getTasks)

  return (
    <div>
      {tasks && <TasksList tasks={tasks} />}

      {isFetching && 'Fetching...'}
      {error && 'Error: ' + error}
    </div>
  )
}

const Task = (props) => (
  <div>
    <input
      type='checkbox' id={props.task.id}
      checked={props.task.isDone} readonly
    />
    {props.task.description}
  </div>
)

const TasksList = (props) => {
  if (!props.tasks?.length) return 'No tasks'
  return props.tasks.map((task, idx) => <Task task={task} key={idx} />)
}

export default MainPage
```

Most of this is just regular React, the only exception being two special `@wasp` imports:
 - `import getTasks from '@wasp/queries/getTasks'` - Gives us our freshly defined Wasp query.
 - `import { useQuery } from '@wasp/queries'` - Gives us Wasp's [useQuery](language/features.md#the-usequery-hook) React hook which is actually just a thin wrapper over [react-query](https://github.com/tannerlinsley/react-query)'s [useQuery](https://react-query.tanstack.com/docs/guides/queries) hook, behaving very similarly while offering some extra integration with Wasp.

While we could call query directly as `getTasks()`, calling it as `useQuery(getTasks)` gives us reactivity- the React component gets re-rendered if the result of the query changes.

With these changes, you should be seeing the text "No tasks" on the screen:

<img alt="Todo App - No Tasks"
     src={useBaseUrl('img/todo-app-no-tasks.png')}
     style={{ border: "1px solid black" }}
/>

Next, let's create some tasks!
