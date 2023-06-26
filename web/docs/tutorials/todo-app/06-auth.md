---
id: 06-auth
title: "Authentication"
---

import useBaseUrl from '@docusaurus/useBaseUrl';

Most of the apps today require some sort of registration and login flows, and Wasp has support for it out of the box, so let's see how to add it to our Todo app!

Let's define a Todo list (luckily we have an app for that now 🙃) to get this done:
- [ ] Add a new entity called `User`.
- [ ] Add `auth` to our `app`.
- [ ] Add `Login` and `Signup` pages.
- [ ] Modify `src/client/MainPage.{jsx,tsx}` so that it requires authentication.
- [ ] Add Prisma relation between `User` and `Task` entities.
- [ ] Modify our queries and actions so that they work only with the tasks belonging to the authenticated user.
- [ ] Add a logout button.

## Adding entity User
First, let's define the `User` entity:
```wasp title="main.wasp"
// ...

entity User {=psl
    id          Int     @id @default(autoincrement())
    username    String  @unique
    password    String
psl=}
```

Run:
```shell
wasp db migrate-dev
```
to propagate the schema change (we added User).

## Defining `app.auth`
Next, we want to tell Wasp that we want full-stack [authentication](language/features.md#authentication--authorization) in our app, and that it should use the `User` entity for it:

```wasp {7-16} title="main.wasp"
app TodoApp {
  wasp: {
    version: "^0.7.0"
  },
  title: "Todo app",

  auth: {
    // Expects entity User to have username and passwords fields.
    userEntity: User,
    methods: {
      // We also support Google, GitHub and email auth, with more on the way!
      usernameAndPassword: {}
    },
    // We'll see how this is used a bit later
    onAuthFailedRedirectTo: "/login"
  }
}
```
What this means for us is that Wasp now offers us:
- Login and Signup forms located at `@wasp/auth/forms/Login` and `@wasp/auth/forms/Signup` paths, ready to be used.
- a `logout()` action.
- a React hook `useAuth()`.
- `context.user` as an argument within queries/actions.

This is a very high-level API for auth which makes it very easy to get started quickly, but is
not very flexible. If you require more control (e.g. want to execute some custom code on the server
during signup, check out the [lower-level auth API](/docs/language/features#lower-level-api).

Ok, that was easy!

To recap, so far we have defined:
- The `User` entity.
- The `app.auth` field, thanks to which Wasp gives us plenty of auth-related functionality.

## Adding Login and Signup pages

When we defined `app.auth` we got login and signup forms generated for us, but now we have to create Login and Signup pages that use them. In our `main.wasp` file we'll add the following:

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

Great, Wasp now knows how to route these and where to find the pages. Now to the React code of the pages:

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


## Updating `MainPage` page to check if the user is authenticated

Now, let's see how we're going to handle the situation when the user is not logged in.
`MainPage` page is a private page and we want users to be able to see it only if they are authenticated.
Wasp allows you to simply enforce private pages using the `authRequired` field:

```wasp {3} title="main.wasp"
// ...
page MainPage {
  authRequired: true,
  component: import Main from "@client/MainPage"
}
```

With `authRequired: true` we declared that page `MainPage` is accessible only to authenticated users.
If an unauthenticated user tries to access route `/` where our page `MainPage` is, they will be redirected to `/login` as specified with the `onAuthFailedRedirectTo` property in `app.auth`.

Also, when `authRequired` is set to `true`, the React component of a page (specified by `component` property within `page`) will be provided `user` object as a prop. It can be accessed like this:

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

Ok, time to try out how this works!

Now, we can start the app again (if it's not still running):
```shell
wasp start
```

Try going to the main page (`/`) of our web app. It will now redirect you to `/login`, where you'll be asked to authenticate. Once you log in or sign up, you will be sent back to `/` and you will see the todo list.

Let's now see how things look in the database! Run:
```shell
wasp db studio
```
<img alt="Database demonstration - password hashing"
     src={useBaseUrl('img/wasp_db_hash_demonstration.gif')}
     style={{ border: "1px solid black" }}
/>

We see there is a user and that its password is already hashed! Wasp took care of this for us.

However, you will notice that if you try logging in with different users and creating tasks, all users are still sharing tasks.
That is because we did not yet update queries and actions to work only on the current user's tasks, so let's do that next!

## Defining User-Task relation in entities

First, let's define a one-to-many relation between User and Task (check the [prisma docs on relations](https://www.prisma.io/docs/reference/tools-and-interfaces/prisma-schema/relations)):
```wasp {6,13-14} title="main.wasp"
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

We modified entities by adding the User-Task relation, so let's run
```shell
wasp db migrate-dev
```
to create a database schema migration and apply it to the database.

:::note
We made `user` and `userId` in `Task` optional (via `?`) because that allows us to keep the existing tasks, which don't have a user assigned, in the database.
This is not recommended because it allows an unwanted state in the database (what is the purpose of the task not belonging to anybody?) and normally we would not make these fields optional.
Instead, we would do a data migration to take care of those tasks, even if it means just deleting them all.
However, for this tutorial, for the sake of simplicity, we will stick with this.
:::

## Updating operations to forbid access to non-authenticated users

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

```ts {3,11,17,28,31,32} title="src/server/actions.ts"
import { Task } from "@wasp/entities"
import { CreateTask, UpdateTask } from "@wasp/actions/types"
import HttpError from '@wasp/core/HttpError.js'

type CreateTaskPayload = Pick<Task, "description">

export const createTask: CreateTask<CreateTaskPayload, Task> = async (
  args,
  context
) => {
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

export const updateTask: UpdateTask<UpdateTaskPayload, { count: number }> = async (
  { id, isDone },
  context
) => {
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

Right, that should be it!

Run (or just continue running):
```shell
wasp start
```
and everything should work as expected now! Each user has their own tasks only they can see and edit.

Try playing around with our app, adding a few users and some tasks. Then run:
```shell
wasp db studio
```
<img alt="Database demonstration"
     src={useBaseUrl('img/wasp_db_demonstration.gif')}
     style={{ border: "1px solid black" }}
/>

You will see that each user has their own tasks, just as we specified in our code!

## Logout button

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
