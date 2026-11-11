---
title: "Tasks: run administrative scripts from the terminal"
sidebar_label: Tasks
description: Define administrative tasks in Wasp, like creating an admin user or backfilling data, and run them from the terminal during development and in production.
---

import { CardLink } from '@site/src/components/CardLink'
import ReferencingCodeFromSrcNote from '../_referencing-code-from-src-note.md'

# Tasks

Apps often need some work done by hand, outside of any request: creating the first admin user, backfilling a new column, or cleaning up old data. **Tasks** are server functions for this kind of work, which you run from the terminal.

A Task has access to the same things as the rest of your server code: your Entities, your Operations, your environment variables, and so on.

## Defining a Task

### Declaring the Task in the Wasp Spec

Declare a Task with `task` in your Wasp file:

```ts title="main.wasp.ts"
import { app, task } from "@wasp/spec"
import { createAdmin } from "./src/tasks" with { type: "ref" }

export default app({
  // ...
  spec: [
    task(createAdmin, {
      entities: ["User"],
      description: "Makes the user with the given email an admin.",
    }),
  ],
})
```

<ReferencingCodeFromSrcNote />

The Task's name is the name of its function, here `createAdmin`. The `description` is optional, and shows up when you [list the Tasks](#listing-tasks).

### Implementing a Task

A Task's function receives two arguments:

- `args`, the arguments you pass after the Task's name on the command line, as an array of strings.
- `context`, with the Prisma API for each Entity in the Task's `entities` field, under `context.entities`.

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="src/tasks.js"
    import { parseArgs } from "node:util"

    export const createAdmin = async (args, context) => {
      const { positionals, values } = parseArgs({
        args,
        allowPositionals: true,
        options: { "dry-run": { type: "boolean" } },
      })
      const [email] = positionals

      if (values["dry-run"]) {
        console.log(`Would make ${email} an admin.`)
        return
      }

      await context.entities.User.update({
        where: { email },
        data: { isAdmin: true },
      })
      console.log(`${email} is now an admin.`)
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="src/tasks.ts"
    import { parseArgs } from "node:util"
    import type { CreateAdmin } from "wasp/server/tasks"

    export const createAdmin: CreateAdmin = async (args, context) => {
      const { positionals, values } = parseArgs({
        args,
        allowPositionals: true,
        options: { "dry-run": { type: "boolean" } },
      })
      const [email] = positionals

      if (values["dry-run"]) {
        console.log(`Would make ${email} an admin.`)
        return
      }

      await context.entities.User.update({
        where: { email },
        data: { isAdmin: true },
      })
      console.log(`${email} is now an admin.`)
    }
    ```

    Wasp generates a type for each Task, named after its function in PascalCase (`CreateAdmin` for `createAdmin`), which types the `args` and the `context.entities`.
  </TabItem>
</Tabs>

We use Node.js's built-in [`parseArgs`](https://nodejs.org/api/util.html#utilparseargsconfig) to parse the arguments, but you can use any library you like.

If the function throws an error, the command prints it and exits with a non-zero exit code, so you can use Tasks in scripts and CI.

## Running Tasks in Development

### Listing Tasks

Run `wasp task list` to see all the Tasks in your app, with their descriptions:

```
$ wasp task list
createAdmin  Makes the user with the given email an admin.
```

### Running a Task

Run `wasp task run <name>`, followed by any arguments for the Task:

```
wasp task run createAdmin admin@example.com --dry-run
```

Wasp passes everything after the Task's name to the Task, including flags like `--dry-run`.

The Task runs against your development database, with the environment variables from your [`.env.server`](../advanced/env-vars.md) file, and from your project's root directory.

:::tip Running a Task against a different database
Like with other `wasp` commands, you can set the `DATABASE_URL` inline to run a Task on a specific database:

```bash
DATABASE_URL=<my-db-url> wasp task run createAdmin admin@example.com
```
:::

## Running Tasks in Production

When you [build your app](../deployment/overview.md), Wasp includes your Tasks in the server's Docker image. To run a Task, run the `task` npm script inside the server's container:

```bash
npm run task -- createAdmin admin@example.com
```

The `--` separates npm's own options from the Task's name and arguments. Wasp runs the Task with the same environment variables as the server, so it connects to the production database.

How you open a shell in the running container depends on your hosting provider. For example, on [Fly.io](https://fly.io/docs/flyctl/ssh-console/):

```bash
fly ssh console --app <server-app-name> --command "sh -c 'cd /app/.wasp/out/server && npm run task -- createAdmin admin@example.com'"
```

## API Reference

### `task` specification

<CardLink
  to="../api/@wasp/spec#task-1"
  kind="api"
  title="task"
  description="All the options for defining a task in the Wasp Spec."
/>

### JavaScript API

<CardLink
  to="../api/wasp/server/tasks"
  kind="api"
  title="wasp/server/tasks"
  description="The types for implementing your app's Tasks."
/>
