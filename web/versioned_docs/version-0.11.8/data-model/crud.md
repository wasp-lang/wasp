---
title: Automatic CRUD
---

import { Required } from '@site/src/components/Tag';
import { ShowForTs } from '@site/src/components/TsJsHelpers';
import ImgWithCaption from '@site/blog/components/ImgWithCaption'

If you have a lot of experience writing full-stack apps, you probably ended up doing some of the same things many times: listing data, adding data, editing it, and deleting it.

Wasp makes handling these boring bits easy by offering a higher-level concept called Automatic CRUD.

With a single declaration, you can tell Wasp to automatically generate server-side logic (i.e., Queries and Actions) for creating, reading, updating and deleting [Entities](../data-model/entities). As you update definitions for your Entities, Wasp automatically regenerates the backend logic.

:::caution Early preview
This feature is currently in early preview and we are actively working on it. Read more about [our plans](#future-of-crud-operations-in-wasp) for CRUD operations.
:::

## Overview

Imagine we have a `Task` entity and we want to enable CRUD operations for it.

```wasp title="main.wasp"
entity Task {=psl
  id          Int @id @default(autoincrement())
  description String
  isDone      Boolean
psl=}
```

We can then define a new `crud` called `Tasks`.

We specify to use the `Task` entity and we enable the `getAll`, `get`, `create` and `update` operations (let's say we don't need the `delete` operation).

```wasp title="main.wasp"
crud Tasks {
  entity: Task,
  operations: {
    getAll: {
      isPublic: true, // by default only logged in users can perform operations
    },
    get: {},
    create: {
      overrideFn: import { createTask } from "@server/tasks.js",
    },
    update: {},
  },
}
```

1. It uses default implementation for `getAll`, `get`, and `update`,
2. ... while specifying a custom implementation for `create`.
3. `getAll` will be public (no auth needed), while the rest of the operations will be private.

Here's what it looks like when visualized:

<ImgWithCaption alt="Automatic CRUD with Wasp" source="img/crud_diagram.png" caption="Visualization of the Tasks crud declaration"/>

We can now use the CRUD queries and actions we just specified in our client code.

Keep reading for an example of Automatic CRUD in action, or skip ahead for the [API Reference](#api-reference)

## Example: A Simple TODO App

Let's create a full-app example that uses automatic CRUD. We'll stick to using the `Task` entity from the previous example, but we'll add a `User` entity and enable [username and password](../auth/username-and-pass) based auth.

<ImgWithCaption alt="Automatic CRUD with Wasp" source="img/crud-guide.gif" caption="We are building a simple tasks app with username based auth"/>

### Creating the App

We can start by running `wasp new tasksCrudApp` and then adding the following to the `main.wasp` file:

```wasp title="main.wasp"
app tasksCrudApp {
  wasp: {
    version: "^0.11.0"
  },
  title: "Tasks Crud App",

  // We enabled auth and set the auth method to username and password
  auth: {
    userEntity: User,
    methods: {
      usernameAndPassword: {},
    },
    onAuthFailedRedirectTo: "/login",
  },
}

entity User {=psl
  id       Int @id @default(autoincrement())
  username String @unique
  password String
  tasks    Task[]
psl=}

// We defined a Task entity on which we'll enable CRUD later on
entity Task {=psl
  id          Int @id @default(autoincrement())
  description String
  isDone      Boolean
  userId      Int
  user        User @relation(fields: [userId], references: [id])
psl=}

// Tasks app routes
route RootRoute { path: "/", to: MainPage }
page MainPage {
  component: import { MainPage } from "@client/MainPage.jsx",
  authRequired: true,
}

route LoginRoute { path: "/login", to: LoginPage }
page LoginPage {
  component: import { LoginPage } from "@client/LoginPage.jsx",
}

route SignupRoute { path: "/signup", to: SignupPage }
page SignupPage {
  component: import { SignupPage } from "@client/SignupPage.jsx",
}
```

We can then run `wasp db migrate-dev` to create the database and run the migrations.

### Adding CRUD to the `Task` Entity ✨

Let's add the following `crud` declaration to our `main.wasp` file:

```wasp title="main.wasp"
// ...

crud Tasks {
  entity: Task,
  operations: {
    getAll: {},
    create: {
      overrideFn: import { createTask } from "@server/tasks.js",
    },
  },
}
```

You'll notice that we enabled only `getAll` and `create` operations. This means that only these operations will be available.

We also overrode the `create` operation with a custom implementation. This means that the `create` operation will not be generated, but instead, the `createTask` function from `@server/tasks.js` will be used.

### Our Custom `create` Operation

Here's the `src/server/tasks.{js,ts}` file:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title=src/server/tasks.js {15-20}
import HttpError from '@wasp/core/HttpError.js'

export const createTask = async (args, context) => {
  if (!context.user) {
    throw new HttpError(401, 'User not authenticated.')
  }

  const { description, isDone } = args
  const { Task } = context.entities

  return await Task.create({
    data: {
      description,
      isDone,
      // Connect the task to the user that is creating it
      user: {
        connect: {
          id: context.user.id,
        },
      },
    },
  })
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title=src/server/tasks.ts {23-28}
import type { CreateAction } from '@wasp/crud/Tasks'
import type { Task } from '@wasp/entities'
import HttpError from '@wasp/core/HttpError.js'

type CreateTaskInput = { description: string; isDone: boolean }

export const createTask: CreateAction<CreateTaskInput, Task> = async (
  args,
  context
) => {
  if (!context.user) {
    throw new HttpError(401, 'User not authenticated.')
  }

  const { description, isDone } = args
  const { Task } = context.entities

  return await Task.create({
    data: {
      description,
      isDone,
      // Connect the task to the user that is creating it
      user: {
        connect: {
          id: context.user.id,
        },
      },
    },
  })
}
```

</TabItem>
</Tabs>

We made a custom `create` operation because we want to make sure that the task is connected to the user that is creating it.
Automatic CRUD doesn't support this by default (yet!).
Read more about the default implementations [here](#declaring-a-crud-with-default-options).

### Using the Generated CRUD Operations on the Client

And let's use the generated operations in our client code:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="pages/MainPage.jsx"
// highlight-next-line
import { Tasks } from '@wasp/crud/Tasks'
import { useState } from 'react'

export const MainPage = () => {
  // highlight-next-line
  const { data: tasks, isLoading, error } = Tasks.getAll.useQuery()
  // highlight-next-line
  const createTask = Tasks.create.useAction()
  const [taskDescription, setTaskDescription] = useState('')

  function handleCreateTask() {
    createTask({ description: taskDescription, isDone: false })
    setTaskDescription('')
  }

  if (isLoading) return <div>Loading...</div>
  if (error) return <div>Error: {error.message}</div>
  return (
    <div
      style={{
        fontSize: '1.5rem',
        display: 'grid',
        placeContent: 'center',
        height: '100vh',
      }}
    >
      <div>
        <input
          value={taskDescription}
          onChange={(e) => setTaskDescription(e.target.value)}
        />
        <button onClick={handleCreateTask}>Create task</button>
      </div>
      <ul>
        {tasks.map((task) => (
          <li key={task.id}>{task.description}</li>
        ))}
      </ul>
    </div>
  )
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="pages/MainPage.tsx"
// highlight-next-line
import { Tasks } from '@wasp/crud/Tasks'
import { useState } from 'react'

export const MainPage = () => {
  // highlight-next-line
  // Thanks to full-stack type safety, all payload types are inferred
  // highlight-next-line
  // automatically
  // highlight-next-line
  const { data: tasks, isLoading, error } = Tasks.getAll.useQuery()
  // highlight-next-line
  const createTask = Tasks.create.useAction()
  const [taskDescription, setTaskDescription] = useState('')

  function handleCreateTask() {
    createTask({ description: taskDescription, isDone: false })
    setTaskDescription('')
  }

  if (isLoading) return <div>Loading...</div>
  if (error) return <div>Error: {error.message}</div>
  return (
    <div
      style={{
        fontSize: '1.5rem',
        display: 'grid',
        placeContent: 'center',
        height: '100vh',
      }}
    >
      <div>
        <input
          value={taskDescription}
          onChange={(e) => setTaskDescription(e.target.value)}
        />
        <button onClick={handleCreateTask}>Create task</button>
      </div>
      <ul>
        {tasks.map((task) => (
          <li key={task.id}>{task.description}</li>
        ))}
      </ul>
    </div>
  )
}
```

</TabItem>
</Tabs>

And here are the login and signup pages, where we are using Wasp's [Auth UI](../auth/ui) components:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="src/client/LoginPage.jsx"
import { LoginForm } from '@wasp/auth/forms/Login'
import { Link } from 'react-router-dom'

export function LoginPage() {
  return (
    <div
      style={{
        display: 'grid',
        placeContent: 'center',
      }}
    >
      <LoginForm />
      <div>
        <Link to="/signup">Create an account</Link>
      </div>
    </div>
  )
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/client/LoginPage.tsx"
import { LoginForm } from '@wasp/auth/forms/Login'
import { Link } from 'react-router-dom'

export function LoginPage() {
  return (
    <div
      style={{
        display: 'grid',
        placeContent: 'center',
      }}
    >
      <LoginForm />
      <div>
        <Link to="/signup">Create an account</Link>
      </div>
    </div>
  )
}
```

</TabItem>
</Tabs>

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="src/client/SignupPage.jsx"
import { SignupForm } from '@wasp/auth/forms/Signup'

export function SignupPage() {
  return (
    <div
      style={{
        display: 'grid',
        placeContent: 'center',
      }}
    >
      <SignupForm />
    </div>
  )
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/client/SignupPage.tsx"
import { SignupForm } from '@wasp/auth/forms/Signup'

export function SignupPage() {
  return (
    <div
      style={{
        display: 'grid',
        placeContent: 'center',
      }}
    >
      <SignupForm />
    </div>
  )
}
```

</TabItem>
</Tabs>

That's it. You can now run `wasp start` and see the app in action. ⚡️

You should see a login page and a signup page. After you log in, you should see a page with a list of tasks and a form to create new tasks.

## Future of CRUD Operations in Wasp

CRUD operations currently have a limited set of knowledge about the business logic they are implementing.

- For example, they don't know that a task should be connected to the user that is creating it. This is why we had to override the `create` operation in the example above.
- Another thing: they are not aware of the authorization rules. For example, they don't know that a user should not be able to create a task for another user. In the future, we will be adding role-based authorization to Wasp, and we plan to make CRUD operations aware of the authorization rules.
- Another issue is input validation and sanitization. For example, we might want to make sure that the task description is not empty.

CRUD operations are a mechanism for getting a backend up and running quickly, but it depends on the information it can get from the Wasp app. The more information that it can pick up from your app, the more powerful it will be out of the box.

We plan on supporting CRUD operations and growing them to become the easiest way to create your backend. Follow along on [this GitHub issue](https://github.com/wasp-lang/wasp/issues/1253) to see how we are doing.

## API Reference

CRUD declaration work on top of existing entity declaration. We'll fully explore the API using two examples:

1. A basic CRUD declaration that relies on default options.
2. A more involved CRUD declaration that uses extra options and overrides.

### Declaring a CRUD With Default Options

If we create CRUD operations for an entity named `Task`, like this:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
crud Tasks { // crud name here is "Tasks"
  entity: Task,
  operations: {
    get: {},
    getAll: {},
    create: {},
    update: {},
    delete: {},
  },
}
```

Wasp will give you the following default implementations:

**get** - returns one entity based on the `id` field

```js
// ...
// Wasp uses the field marked with `@id` in Prisma schema as the id field.
return Task.findUnique({ where: { id: args.id } })
```

**getAll** - returns all entities

```js
// ...

// If the operation is not public, Wasp checks if an authenticated user
// is making the request.

return Task.findMany()
```

**create** - creates a new entity

```js
// ...
return Task.create({ data: args.data })
```

**update** - updates an existing entity

```js
// ...
// Wasp uses the field marked with `@id` in Prisma schema as the id field.
return Task.update({ where: { id: args.id }, data: args.data })
```

**delete** - deletes an existing entity

```js
// ...
// Wasp uses the field marked with `@id` in Prisma schema as the id field.
return Task.delete({ where: { id: args.id } })
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
crud Tasks { // crud name here is "Tasks"
  entity: Task,
  operations: {
    get: {},
    getAll: {},
    create: {},
    update: {},
    delete: {},
  },
}
```

Wasp will give you the following default implementations:

**get** - returns one entity based on the `id` field

```ts
// ...
// Wasp uses the field marked with `@id` in Prisma schema as the id field.
return Task.findUnique({ where: { id: args.id } })
```

**getAll** - returns all entities

```ts
// ...

// If the operation is not public, Wasp checks if an authenticated user
// is making the request.

return Task.findMany()
```

**create** - creates a new entity

```ts
// ...
return Task.create({ data: args.data })
```

**update** - updates an existing entity

```ts
// ...
// Wasp uses the field marked with `@id` in Prisma schema as the id field.
return Task.update({ where: { id: args.id }, data: args.data })
```

**delete** - deletes an existing entity

```ts
// ...
// Wasp uses the field marked with `@id` in Prisma schema as the id field.
return Task.delete({ where: { id: args.id } })
```

</TabItem>
</Tabs>

:::info Current Limitations
In the default `create` and `update` implementations, we are saving all of the data that the client sends to the server. This is not always desirable, i.e. in the case when the client should not be able to modify all of the data in the entity.

[In the future](#future-of-crud-operations-in-wasp), we are planning to add validation of action input, where only the data that the user is allowed to change will be saved.

For now, the solution is to provide an override function. You can override the default implementation by using the `overrideFn` option and implementing the validation logic yourself.

:::

### Declaring a CRUD With All Available Options

Here's an example of a more complex CRUD declaration:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
crud Tasks { // crud name here is "Tasks"
  entity: Task,
  operations: {
    getAll: {
      isPublic: true, // optional, defaults to false
    },
    get: {},
    create: {
      overrideFn: import { createTask } from "@server/tasks.js", // optional
    },
    update: {},
  },
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
crud Tasks { // crud name here is "Tasks"
  entity: Task,
  operations: {
    getAll: {
      isPublic: true, // optional, defaults to false
    },
    get: {},
    create: {
      overrideFn: import { createTask } from "@server/tasks.js", // optional
    },
    update: {},
  },
}
```

</TabItem>
</Tabs>

The CRUD declaration features the following fields:

- `entity: Entity` <Required />

  The entity to which the CRUD operations will be applied.

- `operations: { [operationName]: CrudOperationOptions }` <Required />

  The operations to be generated. The key is the name of the operation, and the value is the operation configuration.

  - The possible values for `operationName` are:
    - `getAll`
    - `get`
    - `create`
    - `update`
    - `delete`
  - `CrudOperationOptions` can have the following fields:
    - `isPublic: bool` - Whether the operation is public or not. If it is public, no auth is required to access it. If it is not public, it will be available only to authenticated users. Defaults to `false`.
    - `overrideFn: ServerImport` - The import statement of the optional override implementation in Node.js.

#### Defining the overrides

Like with actions and queries, you can define the implementation in a Javascript/Typescript file. The overrides are functions that take the following arguments:

- `args`

  The arguments of the operation i.e. the data sent from the client.

- `context`

  Context contains the `user` making the request and the `entities` object with the entity that's being operated on.

<ShowForTs>

You can also import types for each of the functions you want to override from `@wasp/crud/{crud name}`. The available types are:

- `GetAllQuery`
- `GetQuery`
- `CreateAction`
- `UpdateAction`
- `DeleteAction`

If you have a CRUD named `Tasks`, you would import the types like this:

```ts
import type {
  GetAllQuery,
  GetQuery,
  CreateAction,
  UpdateAction,
  DeleteAction,
} from '@wasp/crud/Tasks'

// Each of the types is a generic type, so you can use it like this:
export const getAllOverride: GetAllQuery<Input, Output> = async (
  args,
  context
) => {
  // ...
}
```

</ShowForTs>

For a usage example, check the [example guide](../data-model/crud#adding-crud-to-the-task-entity-).

#### Using the CRUD operations in client code

On the client, you import the CRUD operations from `@wasp/crud/{crud name}`. The names of the imports are the same as the names of the operations. For example, if you have a CRUD called `Tasks`, you would import the operations like this:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="SomePage.jsx"
import { Tasks } from '@wasp/crud/Tasks'
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="SomePage.tsx"
import { Tasks } from '@wasp/crud/Tasks'
```

</TabItem>
</Tabs>

You can then access the operations like this:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="SomePage.jsx"
const { data } = Tasks.getAll.useQuery()
const { data } = Tasks.get.useQuery({ id: 1 })
const createAction = Tasks.create.useAction()
const updateAction = Tasks.update.useAction()
const deleteAction = Tasks.delete.useAction()
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="SomePage.tsx"
const { data } = Tasks.getAll.useQuery()
const { data } = Tasks.get.useQuery({ id: 1 })
const createAction = Tasks.create.useAction()
const updateAction = Tasks.update.useAction()
const deleteAction = Tasks.delete.useAction()
```

</TabItem>
</Tabs>

All CRUD operations are implemented with [Queries and Actions](../data-model/operations/overview) under the hood, which means they come with all the features you'd expect (e.g., automatic SuperJSON serialization, full-stack type safety when using TypeScript)

---

Join our **community** on [Discord](https://discord.com/invite/rzdnErX), where we chat about full-stack web stuff. Join us to see what we are up to, share your opinions or get help with CRUD operations.
