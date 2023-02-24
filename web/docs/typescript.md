---
title: TypeScript Support
---

# Using Wasp with TypeScript

TypeScript is a programming language that brings static type analysis to JavaScript. It is a superset of JavaScript (i.e., all valid JavaScript programs are valid TypeScript programs) and compiles to JavaScript before running. TypeScript's type system detects common errors at build time (reducing the chance of runtime errors in production) and enables type-based auto-completion in IDEs.

This document assumes you are familiar with TypeScript and primarily focuses on how to use it with Wasp. To learn more about TypeScript itself, we recommend reading [the official docs](https://www.typescriptlang.org/docs/).

The document also assumes a basic understanding of core Wasp features (e.g., Queries, Actions, Entities). Read [our feature docs](https://wasp-lang.dev/docs/language/features) to learn more.


## Migrating your project to TypeScript 
Wasp supports TypeScript out of the box!

Our scaffolding already includes TypeScript, so migrating your project to TypeScript is as simple as changing file extensions and using the language. This approach allows you to gradually migrate your project on a file-by-file basis. 

### Example
Let's first assume your Wasp file contains the following definitions:
```c title=main.wasp
entity Task {=psl
    id          Int     @id @default(autoincrement())
    description String
    isDone      Boolean @default(false)
psl=}

query getTaskInfo {
  fn: import { getTaskInfo } from "@server/queries.js",
  entities: [Task]
}
```
Let's now assume that your `queries.js` file looks something like this:

```javascript title="queries.js"
import HttpError from "@wasp/core/HttpError.js"

function getInfoMessage(task) {
  const isDoneText = task.isDone ? "is done" : "is not done"
  return `Task '${task.description}' is ${isDoneText}.`
}

export const getTaskInfo = async ({ id }, context) => {
  const Task = context.entities.Task
  const task = await Task.findUnique({ where: { id } })
  if (!task) {
    throw new HttpError(404)
  }
  return getInfoMessage(task)
}
```
To migrate this file to TypeScript, all you have to do is:
 1. Change the filename from `queries.js` to `queries.ts`.
 2. Write some types.

Let's start by only providing a basic `getInfoMessage` function. We'll see how to properly type the rest of the file in the following sections.
```typescript title=queries.ts
import HttpError from "@wasp/core/HttpError.js"

// highlight-next-line
function getInfoMessage(task: { isDone: boolean, description: string }): string {
  const isDoneText = task.isDone ? "is done" : "is not done"
  return `Task '${task.description}' is ${isDoneText}.`
}

export const getTaskInfo = async ({ id }, context) => {
  const Task = context.entities.Task
  const task = await Task.findUnique({ where: { id } })
  if (!task) {
    throw new HttpError(404)
  }
  return getInfoMessage(task)
}
``` 
You don't need to change anything inside the `.wasp` file.
:::caution
<!-- This block is mostly duplicated in 03-listing-tasks.md -->
Even when you use TypeScript and your file is called `queries.ts`, you still need to import it using the `.js` extension, like this:
```c
query getTaskInfo {
  fn: import { getTaskInfo } from "@server/queries.js",
  entities: [Task]
}
```

Wasp internally uses `esnext` module resultion, which always requires specifying the extension as `.js` (i.e., the extension used in the emitted JS file). This applies to all `@server` immports (and files on the server in general). It does not apply to client files.

Read more about ES modules in TypeScript [here](https://www.typescriptlang.org/docs/handbook/esm-node.html). If you're interested in the discussion and the reasoning behind this, read about it [in this GitHub issue](https://github.com/microsoft/TypeScript/issues/33588).
:::

## Entity Types 
Instead of manually specifying the types for `isDone` and `description`, we can get them from the `Task` entity type. Wasp will generate types for all entities and let you import them from `"@wasp/entities"`:

```typescript title=queries.ts
import HttpError from "@wasp/core/HttpError.js"
// highlight-next-line
import { Task } from "@wasp/entities"

// highlight-next-line
function getInfoMessage(task: Pick<Task, "isDone" | "description">): string {
  const isDoneText = task.isDone ? "is done" : "is not done"
  return `Task '${task.description}' is ${isDoneText}.`
}

export const getTaskInfo = async ({ id }, context) => {
  const Task = context.entities.Task
  const task = await Task.findUnique({ where: { id } })
  if (!task) {
    throw new HttpError(404)
  }
  return getInfoMessage(task)
}
``` 
By doing this, we've connected the argument type of the `getInfoMessage` function with the `Task` entity. This coupling removes duplication and ensures the function keeps the correct signature even if we change the entity. Of course, the function might throw type errors depending on how we change the entity, but that's precisely what we want!

Don't worry about typing the query function for now. We'll see how to handle this in the next section.

Entity types are also available on the client under the same import:
```tsx title=Main.js
import { Task } from "@wasp/entities"

export function ExamplePage() {}
  const task: Task = {
    id: 123,
    description: "Some random task",
    isDone: false,
  }
  return <div>{task.description}</div>;
}

```
The mentioned type safety mechanisms also apply here: Changing the task entity in our `.wasp` file changes the imported type, which might throw a type error and warn us that our task definition is outdated.


## Backend type support for Queries and Actions
Wasp automatically generates the appropriate types for all operations (i.e., Actions and Queries) you define inside your `.wasp` file. Assuming your `.wasp` file contains the following definition:
```c title=main.wasp
// ...

query GetTaskInfo {
  fn: import { getTaskInfo } from "@server/queries.js",
  entities: [Task]
}
```
Wasp will generate a type called `GetTaskInfo`, which you can use to type the Query's implementation. By assigning the `GetTaskInfo` type to your function, you get the type information for its context. In this case, TypeScript will know that the `context.entities` object must include the `Task` entity. If the Query had auth enabled, it would also know that `context` includes user information.

`GetTaskInfo` can is a generic type that takes two (optional) type arguments:
1. `Input` - The argument (i.e., payload) received by the query function.
2. `Output` - The query function's return type.

Suppose you don't care about typing the Query's inputs and outputs. In that case, you can omit both type arguments, and TypeScript will infer the most general types (i.e., `never` for the input, `unknown` for the output.).

```typescript title=queries.ts
import HttpError from "@wasp/core/HttpError.js"
import { Task } from "@wasp/entities"
// highlight-next-line
import { GetTaskInfo } from "@wasp/queries/types"

function getInfoMessage(task: Pick<Task, "isDone" | "description">): string {
  const isDoneText = task.isDone ? "is done" : "is not done"
  return `Task '${task.description}' is ${isDoneText}.`
}

// Use the type parameters to specify the Query's argument and return types.
// highlight-next-line
export const getTaskInfo: GetTaskInfo<Pick<Task, "id">, string> = async ({ id }, context) => {
  // Thanks to the definition in your .wasp file, the compiler knows the type of
  // `context` (and that it contains the `Task` entity).
  const Task = context.entities.Task

  // Thanks to the first type argument in `GetTaskInfo`, the compiler knows `args`
  // is of type `Pick<Task, "id">`.
  const task = await Task.findUnique({ where: { id } })
  if (!task) {
    throw new HttpError(404)
  }

  // Thanks to the second type argument in `GetTaskInfo`, the compiler knows the
  // function must return a value of type `string`.
  return getInfoMessage(task)
}
```
Everything described above applies to Actions as well.

## Frontend type support for Queries and Actions
### Type support for the `useQuery` hook
To add type support to Queries on the frontend, you can use:
- Entity types imported from `"@wasp/entities"`.
- The generic hook `useQuery<Input, Output, Error>` (read more about this hook [here](/docs/language/features#the-useaction-hook)):
  - `Input` - Use this type argument to specify the type for the **request's payload**.
  - `Output` - Use this type argument to specify the type for the **resposne's payload**.
  - `Error` - Use this type argument to specify the error the Query throws.

Here's how a component that uses the Query the `getTaskInfo` might look like:
```tsx title="TaskInfo.tsx"
import { useQuery } from "@wasp/queries"
import getTaskInfo from "@wasp/queries/getTaskInfo"
import { Task } from "@wasp/entities"

type TaskInfoError = { message: string }
type TaskInfoPayload = Pick<Task, "id">

const TaskInfo = ({ id }: TaskInfoPayload) => {
  const { 
    data: taskInfo, // TypeScript knows this is a `string`.
    isError,        // TypeScript knows this is a `boolean`.
    error,          // TypeScript knows this is a `TaskInfoError`.
  } = useQuery<TaskInfoPayload, string, TaskInfoError>(getTaskInfo, { id })
                           // Typescript knows  `id` must be a string ^
  if (isError) {
    return <div>Error during fetching tasks: {error.message || ''}</div>
  }

  return taskInfo === undefined ? <div>Waiting for info...</div> : <div>{taskInfo}</div>
}
```
### Type support for the `useAction` hook
To add type support to Actions on the frontend, you can use:
- Entity types imported from `"@wasp/entities"`.
- The generic hook `useAction<Input, Output, Error>` (read more about this hook [here](/docs/language/features#the-useaction-hook)):
  - `Input` - This type argument specifies the type for the **request's payload**.
  - `Output` - This type argument specifies the type for the **response's payload**.
  - `Error` - This type argument specifies the error the Query throws.

Assuming the following action definition in your `.wasp` file (and the corresponding implementation in `src/server/actions.js`):
```typescript title=main.wasp
// ...

action addTask {
  fn: import { addTask } from "@server/actions.js"
  entities: [Task]
}
```
Here's how you can use it:
```tsx title=AddTask.tsx
const AddTask = ({ description }: Pick<Task, "description">) => {

  // TypeScript knows `addTaskAction` is a function that expects a value of
  // type `Pick<Task, "description"> and returns a value of type
  // `Promise<Task>`.
  const addTaskAction = useAction<Pick<Task, "description" | "isDone">, Task>(addTask)

  return (
    <div>
      <button
        onClick={() => addTaskAction({ description, isDone: false })}
      >Add unfinished task</button>
      <button
        onClick={() => addTaskAction({ description, isDone: true })}
      >Add finished task</button>
    </div>
  )
}
```

The `useAction` hook also includes support for optimistic updates. Read [the feature docs](/docs/language/features#the-useaction-hook) to understand more about optimistic updates and how to define them in Wasp.

Here's an example that shows how you can use static type checking in their definitions (the example assumes an appropriate action defined in the `.wasp` file and implemented on the server):

```tsx title=Task.tsx
import { useQuery } from '@wasp/queries'
import { OptimisticUpdateDefinition, useAction } from '@wasp/actions'
import updateTaskIsDone from '@wasp/actions/updateTaskIsDone'

type TaskPayload = Pick<Task, "id" | "isDone">

const Task = ({ taskId }: Pick<Task, "id">) => {
  const updateTaskIsDoneOptimistically = useAction<TaskPayload, Task>(updateTaskIsDone, {
    optimisticUpdates: [
      {
        getQuerySpecifier: () => [getTask, { id: taskId }],
        // This query's cache should should never be empty
        updateQuery: ({ isDone }, oldTask) => ({ ...oldTask!, isDone }),
      } as OptimisticUpdateDefinition<TaskPayload, Task>,
      {
        getQuerySpecifier: () => [getTasks],
        updateQuery: (updatedTask, oldTasks) =>
          oldTasks && oldTasks.map(task =>
            task.id === updatedTask.id ? { ...task, ...updatedTask } : task
          ),
      } as OptimisticUpdateDefinition<TaskPayload, Task[]>
    ]
  })
  // ...
}
```
