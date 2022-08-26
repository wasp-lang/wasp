---
title: "Updating tasks"
---

import useBaseUrl from '@docusaurus/useBaseUrl';

The Todo app isn't done if you can't mark a task as done!

For that, we will need to do two things:
1. Implement a Wasp action that updates the task.
2. Modify our React code so it calls that action.

## Action

### Wasp declaration

We declare a Wasp action:
```c title="main.wasp"
// ...

action updateTask {
  fn: import { updateTask } from "@ext/actions.js",
  entities: [Task]
}
```

### JS implementation

We define the JS implementation of the Wasp action in `ext/actions.js`:
```js title="ext/actions.js"
// ...

export const updateTask = async (args, context) => {
  return context.entities.Task.update({
    where: { id: args.taskId },
    data: {
      isDone: args.data.isDone
    }
  })
}
```

## React logic

And we update the React component:
```jsx {2,7-16,23} title="ext/MainPage.js"
// ...
import updateTask from '@wasp/actions/updateTask'

// ...

const Task = (props) => {
  const handleIsDoneChange = async (event) => {
    try {
      await updateTask({
        taskId: props.task.id,
        data: { isDone: event.target.checked }
      })
    } catch (error) {
      window.alert('Error while updating task: ' + error.message)
    }
  }

  return (
    <div>
      <input
        type='checkbox' id={props.task.id}
        checked={props.task.isDone}
        onChange={handleIsDoneChange}
      />
      {props.task.description}
    </div>
  )
}
// ...
```

Awesome! We can now tick this task as done ;).
