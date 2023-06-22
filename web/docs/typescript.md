---
title: TypeScript Support
---

# Using Wasp with TypeScript

TypeScript is a programming language that brings static type analysis to JavaScript. It is a superset of JavaScript (i.e., all valid JavaScript programs are valid TypeScript programs) and compiles to JavaScript before running. TypeScript's type system detects common errors at build time (reducing the chance of runtime errors in production) and enables type-based auto-completion in IDEs.

This document assumes you are familiar with TypeScript and primarily focuses on how to use it with Wasp. To learn more about TypeScript itself, we recommend reading [the official docs](https://www.typescriptlang.org/docs/).

The document also assumes a basic understanding of core Wasp features (e.g., Queries, Actions, Entities). You can read more about these features in [our feature docs](https://wasp-lang.dev/docs/language/features).

Besides allowing you to write your code in TypeScript, Wasp also supports:

- Importing and using Wasp Entity types (on both the server and the client).
- Automatic full-stack type support for Queries and Actions - frontend types are automatically inferred from backend definitions.
- Type-safe generic hooks (`useQuery` and `useAction`) with the accompanying type inference.
- Type-safe optimistic update definitions.

We'll dig into the details of each feature in the following sections. But first, let's see how you can introduce TypeScript to an existing Wasp project.

:::info
To get the best IDE experience, make sure to leave `wasp start` running in the background. Wasp will track the working directory and ensure the generated code/types are up to date with your changes.

Your editor may sometimes report type and import errors even while `wasp start` is running. This happens when the TypeScript Language Server gets out of sync with the current code. If you're using VS Code, you can manually restart the language server by opening the command palette and selecting _"TypeScript: Restart TS Server."_
:::

## Migrating your project to TypeScript

Wasp supports TypeScript out of the box!

Our scaffolding already includes TypeScript, so migrating your project to TypeScript is as simple as changing file extensions and using the language. This approach allows you to gradually migrate your project on a file-by-file basis.

### Example

Let's first assume your Wasp file contains the following definitions:

```wasp title=main.wasp
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

```javascript title="src/server/queries.js"
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

1.  Change the filename from `queries.js` to `queries.ts`.
2.  Write some types.

Let's start by only providing a basic `getInfoMessage` function. We'll see how to properly type the rest of the file in the following sections.

```typescript title=src/server/queries.ts
import HttpError from "@wasp/core/HttpError.js"

// highlight-next-line
function getInfoMessage(task: {
  isDone: boolean
  description: string
}): string {
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

Even when you use TypeScript, and your file is called `queries.ts`, you still need to import it using the `.js` extension:

```wasp
query getTaskInfo {
  fn: import { getTaskInfo } from "@server/queries.js",
  entities: [Task]
}
```

Wasp internally uses `esnext` module resolution, which always requires specifying the extension as `.js` (i.e., the extension used in the emitted JS file). This applies to all `@server` imports (and files on the server in general). This quirk does not apply to client files (the transpiler takes care of it).

Read more about ES modules in TypeScript [here](https://www.typescriptlang.org/docs/handbook/esm-node.html). If you're interested in the discussion and the reasoning behind this, read about it [in this GitHub issue](https://github.com/microsoft/TypeScript/issues/33588).
:::

## Entity Types

Instead of manually specifying the types for `isDone` and `description`, we can get them from the `Task` entity type. Wasp will generate types for all entities and let you import them from `"@wasp/entities"`:

```typescript title=src/server/queries.ts
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

```tsx title=src/client/Main.jsx
import { Task } from "@wasp/entities"

export function ExamplePage() {}
  const task: Task = {
    id: 123,
    description: "Some random task",
    isDone: false,
  }
  return <div>{task.description}</div>
}

```

The mentioned type safety mechanisms also apply here: changing the task entity in our `.wasp` file changes the imported type, which might throw a type error and warn us that our task definition is outdated.

## Backend type support for Queries and Actions

Wasp automatically generates the appropriate types for all Operations (i.e., Actions and Queries) you define inside your `.wasp` file. Assuming your `.wasp` file contains the following definition:

```wasp title=main.wasp
// ...

query GetTaskInfo {
  fn: import { getTaskInfo } from "@server/queries.js",
  entities: [Task]
}
```

Wasp will generate a type called `GetTaskInfo`, which you can use to type the Query's implementation. By assigning the `GetTaskInfo` type to your function, you get the type information for its context. In this case, TypeScript will know the `context.entities` object must include the `Task` entity. If the Query had auth enabled, it would also know that `context` includes user information.

`GetTaskInfo` can is a generic type that takes two (optional) type arguments:

1. `Input` - The argument (i.e., payload) received by the query function.
2. `Output` - The query function's return type.

Suppose you don't care about typing the Query's inputs and outputs. In that case, you can omit both type arguments, and TypeScript will infer the most general types (i.e., `never` for the input, `unknown` for the output.).

```typescript title=src/server/queries.ts
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
:::tip

If don't want to define a new type for the Query's return value, the new `satisfies` keyword will allow TypeScript to infer it automatically:
```typescript
const getFoo = (async (_args, context) => {
  const foos = await context.entities.Foo.findMany()
  return {
    foos,
    message: "Here are some foos!",
    queriedAt: new Date(),
  }
}) satisfies GetFoo
```
From the snippet above, TypeScript knows:
1. The correct type for `context`. 
2. The Query's return type is `{ foos: Foo[], message: string, queriedAt: Date }`.

If you don't need the context, you can skip specifying the Query's type (and arguments):
```typescript
const getFoo = () => {{ name: 'Foo', date: new Date() }}
```

:::

## Frontend type support for Queries and Actions

Wasp supports automatic full-stack type safety à la tRPC. You only need to define the Operation's type on the backend, and the frontend will automatically know how to call it.

### Frontend type support for Queries 
The examples assume you've defined the Query `getTaskInfo` from the previous sections:

```typescript title="src/server/queries.ts"
export const getTaskInfo: GetTaskInfo<Pick<Task, "id">, string> = 
  async ({ id }, context) => {
    // ...
  }
```

Wasp will use the type of `getTaskInfo` to infer the Query's types on the frontend:

```tsx title="src/client/TaskInfo.tsx"
import { useQuery } from "@wasp/queries"
// Wasp knows the type of `getTaskInfo` thanks to your backend definition.
// highlight-next-line
import getTaskInfo from "@wasp/queries/getTaskInfo"

export const TaskInfo = () => {
  const {
    // TypeScript knows `taskInfo` is a `string | undefined` thanks to the
    // backend definition.
    data: taskInfo,
    // TypeScript also knows `isError` is a `boolean`.
    isError,
    // TypeScript knows `error` is of type `Error`.
    error,
    // TypeScript knows `id` must be a `Task["id"]` (i.e., a number) thanks to
    // your backend definition.
    // highlight-next-line
  } = useQuery(getTaskInfo, { id: 1 })

  if (isError) {
    return <div> Error during fetching tasks: {error.message || "unknown"}</div>
  }

  // TypeScript forces you to perform this check.
  return taskInfo === undefined ? (
    <div>Waiting for info...</div>
  ) : (
    <div>{taskInfo}</div>
  )
}
```

### Frontend type support for Actions

Assuming the following action definition in your `.wasp` file

```wasp title=main.wasp
action addTask {
  fn: import { addTask } from "@server/actions.js"
  entities: [Task]
}
```

And its corresponding implementation in `src/server/actions.ts`:

```typescript title=src/server/actions.ts
import { AddTask } from "@wasp/actions/types"

type TaskPayload = Pick<Task, "description" | "isDone">

const addTask: AddTask<TaskPayload, Task> = async (args, context) => {
  // ...
}
```

Here's how to use it on the frontend:
```tsx title=src/client/AddTask.tsx
import { useAction } from "@wasp/actions"
// TypeScript knows `addTask` is a function that expects a value of type
// `TaskPayload` and returns a value of type `Promise<Task>`.
import addTask from "@wasp/queries/addTask"

const AddTask = ({ description }: Pick<Task, "description">) => {
  return (
    <div>
      <button onClick={() => addTask({ description, isDone: false })}>
        Add unfinished task
      </button>
      <button onClick={() => addTask({ description, isDone: true })}>
        Add finished task
      </button>
    </div>
  )
}

```
#### Type support for the `useAction` hook
Type inference also works if you decide to use the action via the `useAction` hook:
```typescript
// addTaskFn is of type (args: TaskPayload) => Task
const addTaskFn = useAction(addTask)
```

The `useAction` hook also includes support for optimistic updates. Read [the feature docs](/docs/language/features#the-useaction-hook) to understand more about optimistic updates and how to define them in Wasp.

Here's an example that shows how you can use static type checking in their definitions (the example assumes an appropriate action defined in the `.wasp` file and implemented on the server):

```tsx title=Task.tsx
import { useQuery } from "@wasp/queries"
import { OptimisticUpdateDefinition, useAction } from "@wasp/actions"
import updateTaskIsDone from "@wasp/actions/updateTaskIsDone"

type TaskPayload = Pick<Task, "id" | "isDone">

const Task = ({ taskId }: Pick<Task, "id">) => {
  const updateTaskIsDoneOptimistically = useAction(
    updateTaskIsDone,
    {
      optimisticUpdates: [
        {
          getQuerySpecifier: () => [getTask, { id: taskId }],
          // This query's cache should should never be empty
          updateQuery: ({ isDone }, oldTask) => ({ ...oldTask!, isDone }),
          // highlight-next-line
        } as OptimisticUpdateDefinition<TaskPayload, Task>,
        {
          getQuerySpecifier: () => [getTasks],
          updateQuery: (updatedTask, oldTasks) =>
            oldTasks &&
            oldTasks.map((task) =>
              task.id === updatedTask.id ? { ...task, ...updatedTask } : task
            ),
          // highlight-next-line
        } as OptimisticUpdateDefinition<TaskPayload, Task[]>,
      ],
    }
  )
  // ...
}
```

## Database seeding

When implementing a seed function in TypeScript, you can import a `DbSeedFn` type via

```ts
import type { DbSeedFn } from "@wasp/dbSeed/types.js"
```

and use it to type your seed function like this:

```ts
export const devSeedSimple: DbSeedFn = async (prismaClient) => { ... }
```
