---
id: 04-creating-tasks
title: "Creating tasks"
---

import useBaseUrl from '@docusaurus/useBaseUrl';
import { ShowForTs, ShowForJs } from '@site/src/components/ShowForTs';

To enable the creation of new tasks, we will need two things:
1. A Wasp action that creates a new task.
2. A React form that calls that action with the new task's data.

## Defining the Action
Creating an action is very similar to creating a query.

### Wasp declaration

We must first declare the Action in `main.wasp`:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...

action createTask {
  fn: import { createTask } from "@server/actions.js",
  entities: [Task]
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

action createTask {
  fn: import { createTask } from "@server/actions.js",
  entities: [Task]
}
```

</TabItem>
</Tabs>


<ShowForJs>

### JavaScript implementation
Let's now define a JavaScript function for our action:

</ShowForJs>

<ShowForTs>

### TypeScript implementation
Let's now define a TypeScript function for our action:

</ShowForTs>

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/server/actions.js"
export const createTask = async (args, context) => {
  return context.entities.Task.create({
    data: { description: args.description }
  })
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">


```ts title="src/server/actions.ts"
import { Task } from "@wasp/entities"
import { CreateTask } from "@wasp/actions/types"

type CreateTaskPayload = Pick<Task, "description">

export const createTask: CreateTask<CreateTaskPayload, Task> = async (
  args,
  context
) => {
  return context.entities.Task.create({
    data: { description: args.description },
  })
}
```
Once again, we've annotated the Action with proper types (using the types `Task` and `CreateTask` Wasp generated for us). Annotating the Action makes the type information automatically available the frontend, giving us automatic **Full-stack type safety**.

</TabItem>
</Tabs>

:::tip
We put the function in a new file `src/server/actions.{js,ts}`, but we could have put it anywhere we wanted! There are no limitations here, as long as the import statement in the Wasp file is correct and the source file is inside the `src/server` folder.
:::

## Invoking the Action on the frontend 


<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx {3,10,41-60} title="src/client/MainPage.jsx"
import getTasks from "@wasp/queries/getTasks"
import createTask from "@wasp/actions/createTask"
import { useQuery } from "@wasp/queries"

const MainPage = () => {
  const { data: tasks, isLoading, error } = useQuery(getTasks)

  return (
    <div>
      <NewTaskForm />

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

const NewTaskForm = () => {
  const handleSubmit = async (event) => {
    event.preventDefault()
    try {
      const target = event.target
      const description = target.description.value
      target.reset()
      await createTask({ description })
    } catch (err: any) {
      window.alert("Error: " + err.message)
    }
  }

  return (
    <form onSubmit={handleSubmit}>
      <input name="description" type="text" defaultValue="" />
      <input type="submit" value="Create task" />
    </form>
  )
}

export default MainPage
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx {1,3,12,43-62} title="src/client/MainPage.tsx"
import { FormEvent } from "react"
import getTasks from "@wasp/queries/getTasks"
import createTask from "@wasp/actions/createTask"
import { useQuery } from "@wasp/queries"
import { Task } from "@wasp/entities"

const MainPage = () => {
  const { data: tasks, isLoading, error } = useQuery(getTasks)

  return (
    <div>
      <NewTaskForm />

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

const NewTaskForm = () => {
  const handleSubmit = async (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault()
    try {
      const target = event.target as HTMLFormElement
      const description = target.description.value
      target.reset()
      await createTask({ description })
    } catch (err: any) {
      window.alert("Error: " + err.message)
    }
  }

  return (
    <form onSubmit={handleSubmit}>
      <input name="description" type="text" defaultValue="" />
      <input type="submit" value="Create task" />
    </form>
  )
}

export default MainPage
```

</TabItem>
</Tabs>

We're calling the `createTask` Action directly this time (i.e., without wrapping it with a hook) because we don't need reactivity. The rest is just regular React code.


<ShowForTs>

Finally, because we've previously annotated the Action's backend implementation with the correct type, Wasp knows that the `createTask` action expects a value of type `{ description: string }` (try changing the argument and reading the error message). Wasp also knows that a call to the `createTask` action returns a `Task`, but we don't need it.

</ShowForTs>

That's it!

Try creating a "Build a Todo App in Wasp" task and see it appear in the list below.
The task is created on the server and also saved in the database. Try refreshing the page or opening it in another browser - you'll see the tasks are still here!

<img alt="Todo App - creating new task"
     src={useBaseUrl('img/todo-app-new-task.png')}
     style={{ border: "1px solid black" }}
/>

## Side note: Automatic invalidation/updating of queries
You will notice that when you create a new task, the list of tasks is automatically updated with that new task, although we have written no code to take care of that! Normally, you would have to do this explicitly, e.g. with `react-query` you would invalidate the `getTasks` query via its key, or would call its `refetch()` method.

The reason why the `getTasks` query automatically updates when the `createTask` action is executed is that Wasp is aware that both of them are working with the `Task` entity, and therefore assumes that the action that operates on `Task` (in this case `createTask`) might have changed the result of the `getTasks` query. Therefore, in the background, Wasp nudges the `getTasks` query to update. This means that **out of the box, Wasp will make sure that all your queries that deal with entities are always in sync with any changes that the actions might have done**.

:::note
While this kind of approach to automatic invalidation of queries is very convenient, it is in some situations wasteful and could become a performance bottleneck as the app grows. In that case, you will be able to override this default behavior and instead provide more detailed (and performant) instructions on how the specific action should affect queries.

Overriding the default behavior is not yet supported but it is something we plan to do and you can track the progress [here](https://github.com/wasp-lang/wasp/issues/63) (or even contribute!).
:::
