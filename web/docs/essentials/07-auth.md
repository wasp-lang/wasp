---
title: Adding Authentication
---

import useBaseUrl from '@docusaurus/useBaseUrl';

Most apps today require some sort of registration and login flows, so Wasp has first-class support for it. Let's add it to our Todo app! First, we'll create a Todo list for what needs to be done (luckily we have an app for this now 🙃).

- [ ] Create a `User` entity.
- [ ] Tell Wasp to use username and password authentication.
- [ ] Add login and signup pages.
- [ ] Update the main page to require authentication.
- [ ] Add a relation between `User` and `Task` entities.
- [ ] Modify our queries and actions so that users can only see and modify their own tasks.
- [ ] Add a logout button.

## Creating a User Entity

Since Wasp manages authentication, it expects certain fields to exist on the `User` entity. Specifically, it expects a unique `username` field and a `password` field, both of which should be strings.

```wasp title="main.wasp"
// ...

entity User {=psl
    id       Int    @id @default(autoincrement())
    username String @unique
    password String
psl=}
```

As we talked about earlier, we have to remember to update the database schema:

```sh
wasp db migrate-dev
```

## Adding Auth to the Project

Next, we want to tell Wasp that we want to use full-stack [authentication](/docs/auth/using-auth) in our app:

```wasp {7-16} title="main.wasp"
app TodoApp {
  wasp: {
    version: "^0.11.0"
  },
  title: "Todo app",

  auth: {
    // Tells Wasp which entity to use for storing users.
    userEntity: User,
    methods: {
      // Enable username and password auth.
      usernameAndPassword: {}
    },
    // We'll see how this is used a bit later.
    onAuthFailedRedirectTo: "/login"
  }
}
```

By doing this, Wasp will create:

- Login and signup forms at `@wasp/auth/forms/Login` and `Wasp/auth/forms/Signup`.
- A `logout()` action.
- A React hook `useAuth()`.
- `context.user` within queries and actions.

This is a very high-level API for auth which makes it easy to get started quickly, but is not very flexible. For more control over authentication (e.g. executing some custom code on the server during signup), check out the [lower-level auth API](/docs/auth/using-auth#lower-level-api).

:::tip
Wasp also supports authentication using Google, GitHub, and email, with more on the way!
:::

## Adding Login and Signup Pages

Wasp creates the login and signup forms for us, but we still need to define the pages to display those forms on. We'll start by declaring the pages in the Wasp file:

```wasp title="main.wasp"
// ...

route SignupRoute { path: "/signup", to: SignupPage }
page SignupPage {
  component: import Signup from "@client/SignupPage"
}

route LoginRoute { path: "/login", to: LoginPage }
page LoginPage {
  component: import Login from "@client/LoginPage"
}
```

Great, Wasp now knows these pages exist! Now, the React code for the pages:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="src/client/LoginPage.jsx"
import { Link } from 'react-router-dom'

import { LoginForm } from '@wasp/auth/forms/Login'

const LoginPage = () => {
  return (
    <>
      <LoginForm/>
      <br/>
      <span>
        I don't have an account yet (<Link to="/signup">go to signup</Link>).
      </span>
    </>
  )
}

export default LoginPage
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/client/LoginPage.tsx"
import { Link } from 'react-router-dom'

import { LoginForm } from '@wasp/auth/forms/Login'

const LoginPage = () => {
  return (
    <>
      <LoginForm/>
      <br/>
      <span>
        I don't have an account yet (<Link to="/signup">go to signup</Link>).
      </span>
    </>
  )
}

export default LoginPage
```
</TabItem>
</Tabs>

The Signup page is very similar to the login one:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="src/client/SignupPage.jsx"
import { Link } from 'react-router-dom'

import { SignupForm } from '@wasp/auth/forms/Signup'

const SignupPage = () => {
  return (
    <>
      <SignupForm/>
      <br/>
      <span>
        I already have an account (<Link to="/login">go to login</Link>).
      </span>
    </>
  )
}

export default SignupPage
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/client/SignupPage.tsx"
import { Link } from 'react-router-dom'

import { SignupForm } from '@wasp/auth/forms/Signup'

const SignupPage = () => {
  return (
    <>
      <SignupForm/>
      <br/>
      <span>
        I already have an account (<Link to="/login">go to login</Link>).
      </span>
    </>
  )
}

export default SignupPage
```
</TabItem>
</Tabs>

## Update the Main Page to Require Auth

We don't want users who are not logged in to access the main page, because they won't be able to create any tasks. So let's make the page private by requiring the user to be logged in:

```wasp {4} title="main.wasp"
// ...

page MainPage {
  authRequired: true,
  component: import Main from "@client/MainPage"
}
```

Now that auth is required for this page, unauthenticated users will be redirected to `/login`, as we specified with `app.auth.onAuthFailedRedirectTo`.

Additionally, when `authRequired` is `true`, the page's React component will be provided a `user` object as prop.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx {1} title="src/client/MainPage.jsx"
const MainPage = ({ user }) => {
    // Do something with the user
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx {3} title="src/client/MainPage.tsx"
import { User } from "@wasp/entities"

const MainPage = ({ user }: { user: User }) => {
    // Do something with the user
}
```
</TabItem>
</Tabs>

Ok, time to test this out! Navigate to the main page (`/`) of the app. You'll get redirected to `/login`, where you'll be asked to authenticate. Since we just added users, you don't have an account yet. Go to the signup page and create one. You'll be sent back to the main page where you will now be able to see the todo list!

Let's checkout what the database looks like. Start the studio:

```sh
wasp db studio
```

<img alt="Database demonstration - password hashing"
     src={useBaseUrl('img/wasp_db_hash_demonstration.gif')}
     style={{ border: "1px solid black" }}
/>

We see there is a user and that its password is already hashed! Wasp takes care of all the tricky details of auth for us.

However, you will notice that if you try logging in as different users and creating some tasks, all users share the same tasks. That's because we haven't yet updated the queries and actions to have per-user tasks. Let's work on that next!

## Defining a User-Task Relation

First, let's define a one-to-many relation between users and tasks (check the [prisma docs on relations](https://www.prisma.io/docs/reference/tools-and-interfaces/prisma-schema/relations)):

```wasp {7,16-17} title="main.wasp"
// ...

entity User {=psl
    id          Int     @id @default(autoincrement())
    username    String  @unique
    password    String
    tasks       Task[]
psl=}

// ...

entity Task {=psl
    id          Int     @id @default(autoincrement())
    description String
    isDone      Boolean @default(false)
    user        User?    @relation(fields: [userId], references: [id])
    userId      Int?
psl=}

// ...
```

As always, we have to update the database:

```sh
wasp db migrate-dev
```

:::note
We made `user` and `userId` in `Task` optional (via `?`) because that allows us to keep the existing tasks, which don't have a user assigned, in the database. This is not recommended because it allows an unwanted state in the database (what is the purpose of the task not belonging to anybody?) and normally we would not make these fields optional. Instead, we would do a data migration to take care of those tasks, even if it means just deleting them all. However, for this tutorial, for the sake of simplicity, we will stick with this.
:::

## Updating Operations to Check Authentication

Next, let's update the queries and actions to forbid access to non-authenticated users and to operate only on the currently logged-in user's tasks:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js {1,4} title="src/server/queries.js"
import HttpError from '@wasp/core/HttpError.js'

export const getTasks = async (args, context) => {
  if (!context.user) {
    throw new HttpError(401)
  }
  return context.entities.Task.findMany(
    { where: { user: { id: context.user.id } } }
  )
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```ts {3,6} title="src/server/queries.ts"
import { Task }  from "@wasp/entities"
import { GetTasks } from "@wasp/queries/types"
import HttpError from '@wasp/core/HttpError.js'

export const getTasks: GetTasks<void, Task[]>  = async (args, context) => {
  if (!context.user) {
    throw new HttpError(401)
  }
  return context.entities.Task.findMany(
    { where: { user: { id: context.user.id } } }
  )
}
```
</TabItem>
</Tabs>


<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js {1,4,8,14,17,18} title="src/server/actions.js"
import HttpError from '@wasp/core/HttpError.js'

export const createTask = async (args, context) => {
  if (!context.user) { throw new HttpError(401) }
  return context.entities.Task.create({
    data: {
      description: args.description,
      user: { connect: { id: context.user.id } }
    }
  })
}

export const updateTask = async (args, context) => {
  if (!context.user) {
    throw new HttpError(401)
  }
  return context.entities.Task.updateMany({
    where: { id: args.taskId, user: { id: context.user.id } },
    data: { isDone: args.data.isDone }
  })
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```ts {3,8-10,14,22-26} title="src/server/actions.ts"
import { Task } from "@wasp/entities"
import { CreateTask, UpdateTask } from "@wasp/actions/types"
import HttpError from '@wasp/core/HttpError.js'

type CreateTaskPayload = Pick<Task, "description">

export const createTask: CreateTask<CreateTaskPayload, Task> = async (args, context) => {
  if (!context.user) {
    throw new HttpError(401)
  }
  return context.entities.Task.create({
    data: {
      description: args.description,
      user: { connect: { id: context.user.id } }
    }
  })
}

type UpdateTaskPayload = Pick<Task, "id" | "isDone">

export const updateTask: UpdateTask<UpdateTaskPayload, { count: number }> = async ({ id, isDone }, context) => {
  if (!context.user) {
    throw new HttpError(401)
  }
  return context.entities.Task.updateMany({
    where: { id: args.taskId, user: { id: context.user.id } },
    data: { isDone: args.data.isDone }
  })
}
```
</TabItem>
</Tabs>

:::note
Due to how Prisma works, we had to convert `update` to `updateMany` in `updateTask` action to be able to specify the user id in `where`.
:::

With these changes, each user should have their own list of tasks that only they can see and edit.

Try playing around, adding a few users and some tasks for each of them. Then open the DB studio:

```sh
wasp db studio
```

<img alt="Database demonstration"
     src={useBaseUrl('img/wasp_db_demonstration.gif')}
     style={{ border: "1px solid black" }}
/>

You will see that each user has their own tasks, just as we specified in our code!

## Logout Button

Last, but not least, let's add the logout functionality:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx {2,10} title="src/client/MainPage.jsx"
// ...
import logout from '@wasp/auth/logout'
//...

const MainPage = () => {
  // ...
  return (
    <div>
      // ...
      <button onClick={logout}>Logout</button>
    </div>
  )
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx {2,10} title="src/client/MainPage.tsx"
// ...
import logout from '@wasp/auth/logout'
//...

const MainPage = () => {
  // ...
  return (
    <div>
      // ...
      <button onClick={logout}>Logout</button>
    </div>
  )
}
```
</TabItem>
</Tabs>

This is it, we have a working authentication system, and our Todo app is multi-user!

## What's Next?

You did it! You've followed along with this tutorial to create a basic Todo app with Wasp! You can find the complete code for the tutorial [here](https://github.com/wasp-lang/wasp/tree/release/examples/tutorials/TodoApp).

You should be ready to learn about more complicated features and go more in-depth with the features already covered. Scroll through the sidebar on the left side of the page to see every feature Wasp has to offer. Or, let your imagination run wild and start building your own app! ✨

Looking for inspiration?

- Get a jump start on your next project with [Starter Templates](/docs/project/starter-templates)!
- Make a real-time app with [Web Sockets](/docs/advanced/web-sockets)!

If you notice that some of the features you'd like to have are missing, or have any other kind of feedback, please write to us on [Discord](https://discord.gg/rzdnErX) or create an issue on [Github](https://github.com/wasp-lang/wasp), so we can learn which features to add/improve next 🙏

Even better, if you would like to contribute or help building the feature, let us know! You can find more details on contributing [here](contributing.md).

Oh, and do **subscribe to our newsletter: https://wasp-lang.dev/#signup**! We usually send one per month, and Matija does his best to unleash his creativity to make them engaging and fun to read :D!