---
title: 6. Modifying Data
---

import useBaseUrl from '@docusaurus/useBaseUrl';
import { ShowForTs, ShowForJs } from '@site/src/components/TsJsHelpers';
import Collapse from '@site/src/components/Collapse';

In the previous section, you learned about using Queries to fetch data.
Let's now learn about Actions so you can add and update tasks in the database.

In this section, you will create:

1. A Wasp Action that creates a new task.
2. A React form that calls that Action when the user creates a task.

## Creating a New Action

Creating an Action is very similar to creating a Query.

### Declaring an Action

We must first declare the Action in `main.wasp`:

```wasp title="main.wasp"
// ...

action createTask {
  fn: import { createTask } from "@src/actions",
  entities: [Task]
}
```

### Implementing an Action

Let's now define a <ShowForJs>JavaScript</ShowForJs><ShowForTs>TypeScript</ShowForTs> function for our `createTask` Action:

```ts title="src/actions.ts" auto-js
import type { Task } from 'wasp/entities'
import type { CreateTask } from 'wasp/server/operations'

type CreateTaskPayload = Pick<Task, 'description'>

export const createTask: CreateTask<CreateTaskPayload, Task> = async (
  args,
  context
) => {
  return context.entities.Task.create({
    data: { description: args.description },
  })
}
```

<ShowForTs>
Once again, we've annotated the Action with the `CreateTask` and `Task` types generated by Wasp. Just like with queries, defining the types on the implementation makes them available on the frontend, giving us **full-stack type safety**.
</ShowForTs>


:::tip
We put the function in a new file `src/actions.{js,ts}`, but we could have put it anywhere we wanted! There are no limitations here, as long as the declaration in the Wasp file imports it correctly and the file is located within `src` directory.
:::

## Invoking the Action on the Client

Start by defining a form for creating new tasks.

```tsx title="src/MainPage.tsx" auto-js
import type { FormEvent } from 'react'
import type { Task } from 'wasp/entities'
import {
  // highlight-next-line
  createTask,
  getTasks,
  useQuery
} from 'wasp/client/operations'

// ... MainPage, TaskView, TaskList ...

// highlight-start
const NewTaskForm = () => {
  const handleSubmit = async (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault()
    try {
      const target = event.target as HTMLFormElement
      const description = target.description.value
      target.reset()
      await createTask({ description })
    } catch (err: any) {
      window.alert('Error: ' + err.message)
    }
  }

  return (
    <form onSubmit={handleSubmit}>
      <input name="description" type="text" defaultValue="" />
      <input type="submit" value="Create task" />
    </form>
  )
}
// highlight-end
```

Unlike Queries, you can call Actions directly (without wrapping them in a hook) because they don't need reactivity. The rest is just regular React code.

<ShowForTs>
  Finally, because we've previously annotated the Action's server implementation with the correct type, Wasp knows that the `createTask` Action expects a value of type `{ description: string }` (try changing the argument and reading the error message). Wasp also knows that a call to the `createTask` Action returns a `Task` but are not using it in this example.
</ShowForTs>

All that's left now is adding this form to the page component:

```tsx title="src/MainPage.tsx" auto-js
import type { FormEvent } from 'react'
import type { Task } from 'wasp/entities'
import { createTask,  getTasks,  useQuery } from 'wasp/client/operations'

const MainPage = () => {
  const { data: tasks, isLoading, error } = useQuery(getTasks)

  return (
    <div>
      // highlight-next-line
      <NewTaskForm />
      {tasks && <TasksList tasks={tasks} />}
      {isLoading && 'Loading...'}
      {error && 'Error: ' + error}
    </div>
  )
}

// ... TaskList, TaskView, NewTaskForm ...
```

Great work!

You now have a form for creating new tasks.

Try creating a "Build a Todo App in Wasp" task and see it appear in the list below. The task is created on the server and saved in the database.

Try refreshing the page or opening it in another browser. You'll see the tasks are still there!

<img alt="Todo App - creating new task" src={useBaseUrl('img/todo-app-new-task.png')} style={{ border: "1px solid black" }} />

<br />

<br />

:::note Automatic Query Invalidation
When you create a new task, the list of tasks is automatically updated to display the new task, even though you haven't written any code that does that! Wasp handles these automatic updates under the hood.

When you declared the `getTasks` and `createTask` operations, you specified that they both use the `Task` entity. So when `createTask` is called, Wasp knows that the data `getTasks` fetches may have changed and automatically updates it in the background. This means that **out of the box, Wasp keeps all your queries in sync with any changes made through Actions**.

This behavior is convenient as a default but can cause poor performance in large apps. While there is no mechanism for overriding this behavior yet, it is something that we plan to include in Wasp in the future. This feature is tracked [here](https://github.com/wasp-lang/wasp/issues/63).
:::

## A Second Action

Our Todo app isn't finished if you can't mark a task as done.

We'll create a new Action to update a task's status and call it from React whenever a task's checkbox is toggled.

Since we've already created one task together, try to create this one yourself. It should be an Action named `updateTask` that receives the task's `id` and its `isDone` status. You can see our implementation below.

<Collapse title="Solution">
  Declaring the Action in `main.wasp`:

  ```wasp title="main.wasp"
  // ...

  action updateTask {
    fn: import { updateTask } from "@src/actions",
    entities: [Task]
  }
  ```

  Implementing the Action on the server:

  ```ts title="src/actions.ts" auto-js
  import type { CreateTask, UpdateTask } from 'wasp/server/operations'

  // ...

  type UpdateTaskPayload = Pick<Task, 'id' | 'isDone'>

  export const updateTask: UpdateTask<UpdateTaskPayload, Task> = async (
    { id, isDone },
    context
  ) => {
    return context.entities.Task.update({
      where: { id },
      data: {
        isDone: isDone,
      },
    })
  }
  ```
</Collapse>

You can now call `updateTask` from the React component:

```tsx title="src/MainPage.tsx" auto-js
import type { FormEvent, ChangeEvent } from 'react'
import type { Task } from 'wasp/entities'
import {
  // highlight-next-line
  updateTask,
  createTask,
  getTasks,
  useQuery,
} from 'wasp/client/operations'


// ... MainPage ...

const TaskView = ({ task }: { task: Task }) => {
  // highlight-start
  const handleIsDoneChange = async (event: ChangeEvent<HTMLInputElement>) => {
    try {
      await updateTask({
        id: task.id,
        isDone: event.target.checked,
      })
    } catch (error: any) {
      window.alert('Error while updating task: ' + error.message)
    }
  }
  // highlight-end

  return (
    <div>
      <input
        type="checkbox"
        id={String(task.id)}
        checked={task.isDone}
        // highlight-next-line
        onChange={handleIsDoneChange}
      />
      {task.description}
    </div>
  )
}

// ... TaskList, NewTaskForm ...
```

Awesome!
You can now mark this task as done.

It's time to make one final addition to your app: supporting multiple users.
