---
title: Automatic CRUD
---

import ImgWithCaption from '../../blog/components/ImgWithCaption'

For a specific [Entity](/docs/language/features#entity), you can tell Wasp to automatically instantiate server-side logic ([Queries](/docs/language/features#query) and [Actions](/docs/language/features#action)) for creating, reading, updating and deleting such entities. 

## Defining new CRUD operations

Let's say we have a `Task` entity. We want to have `getAll` and `get` queries for it, and also `create` and `update` actions. We do this by creating new `crud` declaration in Wasp, named `Tasks`, that uses default implementation for `getAll`, `get` and `update`, while specifying a custom implementation for `create`. We also configured `getAll` to be publicly available (no auth needed).

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

Result of this is that the queries and actions we just specified are now available in our Wasp app!

## Example: simple tasks app

We'll see an example app with auth and CRUD operations for some `Task` entity.

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
  id Int @id @default(autoincrement())
  username String @unique
  password String
  tasks Task[]
psl=}

// We defined a Task entity on which we'll enable CRUD later on
entity Task {=psl
  id Int @id @default(autoincrement())
  description String
  isDone Boolean
  userId Int
  user User @relation(fields: [userId], references: [id])
psl=}

// Tasks app routes
route RootRoute { path: "/", to: MainPage }
page MainPage {
  component: import Main from "@client/MainPage.tsx",
  authRequired: true,
}

route LoginRoute { path: "/login", to: LoginPage }
page LoginPage {
  component: import { LoginPage } from "@client/LoginPage.tsx",
}

route SignupRoute { path: "/signup", to: SignupPage }
page SignupPage {
  component: import { SignupPage } from "@client/SignupPage.tsx",
}
```

We can then run `wasp db migrate-dev` to create the database and run the migrations.

### Adding CRUD to the `Task` entity ✨

We add the following to our Wasp file to enable automatic CRUD for `Task`:

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

You'll notice that we enabled only `getAll` and `create` operations. This means that only these operations will be available. We also overrode the `create` operation with a custom implementation. This means that the `create` operation will not be generated, but instead, the `createTask` function from `@server/tasks.js` will be used.

### Implementing the `create` operation

We have the following implementation in `src/server/tasks.js`:

```ts title="src/server/tasks.ts"
import type { CreateAction } from '@wasp/crud/Tasks'
import type { Task } from '@wasp/entities'
import HttpError from '@wasp/core/HttpError.js';

export const createTask: CreateAction<
  { description: string; isDone: boolean },
   Task,
> = async (args, context) => {
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

We made a custom `create` operation because we want to make sure that the task is connected to the user that is creating it. By default, the `create` operation would not do that. Read more about the [default implementations](/docs/language/features#default-crud-operations-implementations).

### Using the generated CRUD operations

And let's use the generated operations in our client code:

```jsx title="pages/MainPage.jsx"
import { Tasks } from "@wasp/crud/Tasks";
import { useState } from "react";
// Default CSS that comes with Wasp for the main page
import "./Main.css";

export const MainPage = () => {
  const { data: tasks, isLoading, error } = Tasks.getAll.useQuery();
  const createAction = Tasks.create.useAction();
  const [taskDescription, setTaskDescription] = useState("");

  function handleCreateTask() {
    createAction({ description: taskDescription, isDone: false });
    setTaskDescription("");
  }

  if (isLoading) return <div>Loading...</div>;
  if (error) return <div>Error: {error.message}</div>;
  return (
    <div className="container">
      <main>
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
      </main>
    </div>
  );
};

```

And here are the login and signup pages:

```jsx title="src/client/LoginPage.jsx"
import { LoginForm } from '@wasp/auth/forms/Login'
import { Link } from "react-router-dom"

export function LoginPage() {
  return (
    <div>
      <h1>Login</h1>
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
    <div>
      <h1>Signup</h1>
      <SignupForm />
    </div>
  )
}
```

That's it. You can now run `wasp start` and see the app in action.

You should see a login page and a signup page. After you log in, you should see a page with a list of tasks and a form to create new tasks.

### Future of CRUD operations in Wasp

CRUD operations currently have a limited set of knowledge about the business logic they are implementing. For example, they don't know that a task should be connected to the user that is creating it. This is why we had to override the `create` operation in the example above.

Another thing, they are not aware of the authorization rules. For example, they don't know that a user should not be able to create a task for another user. In the future, we will be adding role-based authorization to Wasp, and we plan to make CRUD operations aware of the authorization rules.

Another issue is input validation and sanitization. For example, we might want to make sure that the task description is not empty.

To conclude, CRUD operations are a mechanism for getting a backend up and running quickly, but it depends on the information it can get from the Wasp app. The more information that it can pick up from your app, the more powerful it will be out of the box. We plan on supporting CRUD operations and growing them to become the easiest way to create your backend.