---
id: 05-updating-tasks
title: "Updating tasks"
---

import useBaseUrl from '@docusaurus/useBaseUrl';
import { ShowForTs, ShowForJs } from '@site/src/components/ShowForTs';

Our Todo app isn't finished if you we can't mark a task as finished!

To do that, we'll need to do two things:
1. Implement a Wasp Action for toggling a task's `isDone` state.
2. Call this Action from React whenever the user toggles a checkbox.

## Defining the Action

### Wasp declaration

Let's first define the Action in `main.wasp`:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...

action updateTask {
  fn: import { updateTask } from "@server/actions.js",
  entities: [Task]
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

action updateTask {
  fn: import { updateTask } from "@server/actions.js",
  entities: [Task]
}
```

</TabItem>
</Tabs>



<ShowForJs>

### JavaScript implementation
Let's now define the Action's JavaScript implementation in `src/server/actions.js`:

</ShowForJs>

<ShowForTs>

### TypeScript implementation
Let's now define the Action's JavaScript implementation in `src/server/actions.ts`:

</ShowForTs>

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/server/actions.js"
// ...

export const updateTask = async ({ id, isDone }, context) => {
  return context.entities.Task.update({
    where: { id  },
    data: {
      isDone: isDone,
    },
  })
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/server/actions.ts"
// highlight-next-line
import { CreateTask, UpdateTask } from "@wasp/actions/types"

// ...

type UpdateTaskPayload = Pick<Task, "id" | "isDone">

export const updateTask: UpdateTask<UpdateTaskPayload, Task> = async (
  { id, isDone },
  context
) => {
  return context.entities.Task.update({
    where: { id  },
    data: {
      isDone: isDone,
    },
  })
}
```

</TabItem>
</Tabs>

## Invoking the Action on the frontend 

Finally, all that's left to do is call the Action from the React component:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx {2,7-16,24} title="src/client/MainPage.jsx"
// ...
import updateTask from '@wasp/actions/updateTask'

// ...

const Task = ({ task }) => {
  const handleIsDoneChange = async (event) => {
    try {
      await updateTask({
        id: task.id,
        isDone: event.target.checked,
      })
    } catch (error: any) {
      window.alert("Error while updating task: " + error.message)
    }
  }

  return (
    <div>
      <input
        type="checkbox"
        id={String(task.id)}
        checked={task.isDone}
        onChange={handleIsDoneChange}
      />
      {task.description}
    </div>
  )
}
// ...
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx {2,4,9-18,26} title="src/client/MainPage.tsx"
// ...
import { FormEvent, ChangeEvent } from "react"
// ...
import updateTask from '@wasp/actions/updateTask'

// ...

const Task = ({ task }: { task: Task }) => {
  const handleIsDoneChange = async (event: ChangeEvent<HTMLInputElement>) => {
    try {
      await updateTask({
        id: task.id,
        isDone: event.target.checked,
      })
    } catch (error: any) {
      window.alert("Error while updating task: " + error.message)
    }
  }

  return (
    <div>
      <input
        type="checkbox"
        id={String(task.id)}
        checked={task.isDone}
        onChange={handleIsDoneChange}
      />
      {task.description}
    </div>
  )
}
// ...
```

</TabItem>
</Tabs>

Awesome! We can now tick this task as done ;).
