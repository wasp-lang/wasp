---
title: Automatic CRUD
---

import ImgWithCaption from '../../blog/components/ImgWithCaption'

Wasp supports automatic action and query generation based of your Prisma models. It will generate the Create, Read (Read All), Update and Delete actions and queries for each model. You can then use them in your client code.

### How to declare a CRUD operation?

The CRUD declaration works on top of an existing entity you defined in your Wasp file.

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

### Simple tasks app

We'll see an example app with some auth and CRUD operations for a `Task` entity.

<ImgWithCaption alt="Automatic CRUD with Wasp" source="img/crud-guide.gif" caption="We are building a simple tasks app with username based auth"/>

We can start by running `wasp new tasksCrudApp` and then we'll add the following to our `main.wasp` file:

```wasp title="main.wasp"
app tasksCrudApp {
  wasp: {
    version: "^0.11.0"
  },
  title: "Tasks Crud App",
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

entity Task {=psl
  id Int @id @default(autoincrement())
  description String
  isDone Boolean
  userId Int
  user User @relation(fields: [userId], references: [id])
psl=}

// Main and auth routes
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

#### Enabling automatic CRUD

After that, we add the following to our Wasp file to enable automatic CRUD for `Task`:

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

You'll notice that we enabled only `getAll` and `create` operations. This means that only these operations will be generated. We also overrode the `create` operation with a custom function. This means that the `create` operation will not be generated, but instead, the `createTask` function from `@server/tasks.js` will be used.

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

We overrode the `create` operation because we want to make sure that the task is connected to the user that is creating it. By default the `create` operation would not do that.

And let's use the generated operations in our client code:

```jsx title="pages/MainPage.jsx"
import { Tasks } from "@wasp/crud/Tasks";
import { useState } from "react";
// Default CSS that comes with Wasp
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

You should see a login page and a signup page. After you login, you should see a page with a list of tasks and a form to create new tasks.