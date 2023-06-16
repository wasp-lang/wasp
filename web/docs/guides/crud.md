---
title: Automatic CRUD
---

import ImgWithCaption from '../../blog/components/ImgWithCaption'

For some [Entity](/docs/language/features#entity), you can tell Wasp to automatically generate server-side logic ([Queries](/docs/language/features#query) and [Actions](/docs/language/features#action)) for creating, reading, updating and deleting such entities. As your entities update, Wasp will automatically regenerate the backend logic.

:::caution Early preview
This feature is currently in early preview and we are actively working on it. Read more about [our plans](/docs/language/features#crud-operations-on-top-of-entities) for CRUD operations.
:::

## Defining new CRUD operations

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

1. It uses default implementation for `getAll`, `get` and `update`,
2. ... while specifying a custom implementation for `create`. 
3. `getAll` will be public (no auth needed), while the rest of the operations will be private.

Here's what it looks like when visualized:

<ImgWithCaption alt="Automatic CRUD with Wasp" source="img/crud_diagram.png" caption="Visualization of the Tasks crud declaration"/>

We can now use the CRUD queries and actions we just specified in our client code.

## Example: simple TODO app

Let's create a full app example that uses automatic CRUD. We'll stick to using the `Task` entity from the previous example, but we'll add a `User` entity and enable [username and password](/docs/language/features#username-and-password) based auth.

<ImgWithCaption alt="Automatic CRUD with Wasp" source="img/crud-guide.gif" caption="We are building a simple tasks app with username based auth"/>

### Creating the app

We can start by running `wasp new tasksCrudApp` and then we'll add the following to our `main.wasp` file:

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

### Adding CRUD to the `Task` entity ✨

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

### Our custom `create` operation

Here's the `src/server/tasks.js` file:

```ts title="src/server/tasks.ts" {20-25}
import type { CreateAction } from '@wasp/crud/Tasks'
import type { Task } from '@wasp/entities'
import HttpError from '@wasp/core/HttpError.js'

type Input = { description: string; isDone: boolean }
type Output = Task

export const createTask: CreateAction<Input, Output> = async (args, context) => {
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

We made a custom `create` operation because we want to make sure that the task is connected to the user that is creating it. In the current iteration of CRUD operations that is not supported by default. Read more about the [default implementations](/docs/language/features#which-operations-are-supported).

### Using the generated CRUD operations on the client

And let's use the generated operations in our client code:

```jsx title="pages/MainPage.jsx" {1,5-6}
import { Tasks } from '@wasp/crud/Tasks'
import { useState } from 'react'

export const MainPage = () => {
  const { data: tasks, isLoading, error } = Tasks.getAll.useQuery()
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

And here are the login and signup pages, where we are using Wasp's [Auth UI](/docs/guides/auth-ui) components:

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

That's it. You can now run `wasp start` and see the app in action ⚡️

You should see a login page and a signup page. After you log in, you should see a page with a list of tasks and a form to create new tasks.

### Future of CRUD operations in Wasp

CRUD operations currently have a limited set of knowledge about the business logic they are implementing.

- For example, they don't know that a task should be connected to the user that is creating it. This is why we had to override the `create` operation in the example above.
- Another thing: they are not aware of the authorization rules. For example, they don't know that a user should not be able to create a task for another user. In the future, we will be adding role-based authorization to Wasp, and we plan to make CRUD operations aware of the authorization rules.
- Another issue is input validation and sanitization. For example, we might want to make sure that the task description is not empty.

CRUD operations are a mechanism for getting a backend up and running quickly, but it depends on the information it can get from the Wasp app. The more information that it can pick up from your app, the more powerful it will be out of the box. 

We plan on supporting CRUD operations and growing them to become the easiest way to create your backend. Follow along on [this Github issue](https://github.com/wasp-lang/wasp/issues/1253) to see how we are doing.

---

Join our **community** on [Discord](https://discord.com/invite/rzdnErX), where we chat about full-stack web stuff. Join us to see what we are up to, share your opinions or get help with CRUD operations.
