---
title: TypeScript Support
---

import TypescriptServerNote from '../_TypescriptServerNote.md'

# TypeScript support

TypeScript is a programming language that adds static type analysis to JavaScript.
It is a superset of JavaScript, which means all JavaScript code is valid TypeScript code.
It also compiles to JavaScript before running.

TypeScript's type system helps catch errors at build time (this reduces runtime errors), and provides type-based auto-completion in IDEs.

Each Wasp feature includes TypeScript documentation.

If you're starting a new project and want to use TypeScript, you don't need to do anything special.
Just follow the feature docs you are interested in, and they will tell you everything you need to know.
We recommend you start by going through [the tutorial](../tutorial/01-create.md).

To migrate an existing Wasp project from JavaScript to TypeScript, follow this guide.

## Migrating your project to TypeScript

Since Wasp ships with out-of-the-box TypeScript support, migrating your project is as simple as changing file extensions and using the language.
This approach allows you to gradually migrate your project on a file-by-file basis.

We will first show you how to migrate a single file and then help you generalize the procedure to the rest of your project.

### Migrating a single file

Assuming your `schema.prisma` file defines the `Task` entity:

```prisma title="schema.prisma"
// ...

model Task {
  id          Int @id @default(autoincrement())
  description String
  isDone      Boolean
}
```

And your `main.wasp` file defines the `getTaskInfo` query:

```wasp title="main.wasp"
query getTaskInfo {
  fn: import { getTaskInfo } from "@src/queries",
  entities: [Task]
}
```

We will show you how to migrate the following `queries.js` file:

```javascript title="src/queries.js"
import HttpError from 'wasp/server'

function getInfoMessage(task) {
  const isDoneText = task.isDone ? 'is done' : 'is not done'
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
2. Write some types (and optionally use some of Wasp's TypeScript features).

<Tabs>
  <TabItem value="before" label="Before">
    ```javascript title="src/queries.js"
    import HttpError from '@wasp/core/HttpError.js'

    function getInfoMessage(task) {
      const isDoneText = task.isDone ? 'is done' : 'is not done'
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
  </TabItem>

  <TabItem value="after" label="After">
    ```typescript title="src/queries.ts"
    import HttpError from 'wasp/server'
    // highlight-next-line
    import { type Task } from '@wasp/entities'
    // highlight-next-line
    import { type GetTaskInfo } from '@wasp/server/operations'

    // highlight-next-line
    function getInfoMessage(task: Pick<Task, 'isDone' | 'description'>): string {
      const isDoneText = task.isDone ? 'is done' : 'is not done'
      return `Task '${task.description}' is ${isDoneText}.`
    }

    // highlight-next-line
    export const getTaskInfo: GetTaskInfo<Pick<Task, 'id'>, string> = async (
      { id },
      context
    ) => {
      const Task = context.entities.Task

      const task = await Task.findUnique({ where: { id } })
      if (!task) {
        throw new HttpError(404)
      }

      return getInfoMessage(task)
    }
    ```
  </TabItem>
</Tabs>

Your code is now processed by TypeScript and uses several of Wasp's TypeScript-specific features:

- `Task` - A type that represents the `Task` entity. Using this type connects your data to the model definitions in the `schema.prisma` file. Read more about this feature [here](../data-model/entities).
- `GetTaskInfo<...>` - A generic type Wasp automatically generates to give you type
  support when implementing the Query. Thanks to this type, the compiler knows:

  - The type of the `context` object.
  - The type of `args`.
  - The Query's return type.

  And gives you Intellisense and type-checking. Read more about this feature [here](../data-model/operations/queries#implementing-queries).

You don't need to change anything inside the `.wasp` file.

### Migrating the rest of the project

You can migrate your project gradually - on a file-by-file basis.

When you want to migrate a file, follow the procedure outlined above:

1. Change the file's extension.
2. Fix the type errors.
3. Read the Wasp docs and decide which TypeScript features you want to use.

<TypescriptServerNote />
