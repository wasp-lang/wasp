---
title: Username & Password
---

import useBaseUrl from '@docusaurus/useBaseUrl';

# Username & Password

### Configuration

To get started with a simple Username & Password Auth strategy, you'll need to add the Auth object with the following configuration to your `main.wasp` file:
```c title="main.wasp"
app Example {
  wasp: {
    version: "^0.11.0"
  },

  title: "Example",

  auth: {
    userEntity: User,
    methods: {
      usernameAndPassword: {},
    },
    onAuthFailedRedirectTo: "/login"
  }
}

// Wasp requires the userEntity to have at least the following fields
entity User {=psl
    id                        Int           @id @default(autoincrement())
    username                  String        @unique
    password                  String
psl=}

// ...
```
For more info on the specific fields, check out this [Auth](/docs/language/features#authentication--authorization) section of the docs.

If you're adding a new entity to your `.wasp` file for the first time, make sure to create the table for it in your database:
```shell
wasp db migrate-dev
```

You'll also need to add these environment variables to your `.env.server` file at the root of your project:

```bash title=".env.server"
JWT_SECRET=random-string-at-least-32-characters-long.
```

With `auth` now defined, Wasp offers a number of handy features out of the box:
- ["AuthUI" Login and Signup forms](/docs/guides/auth-ui) located at `@wasp/auth/forms/Login` and `@wasp/auth/forms/Signup` paths, ready to be styled and used.
- The `logout()` action.
- The `useAuth()` hook to access the logged-in user client-side.
- The `context.user` object as an argument server-side within [Operations](/docs/language/features#queries-and-actions-aka-operations).

:::tip Customizing Auth
This is a very high-level API for auth which makes it very easy to get started quickly, but is
not very flexible. If you require more control (e.g. want to execute some custom code on the server
during signup, check out the [lower-level auth API](/docs/language/features#lower-level-api).
:::

### Client-side

To access the logged-in user client-side, you have two options:

1. You can use the `user` object that Wasp passes to all pages by default:

```jsx
const Page = ({ user }) => {
  const username = user.username

  //...
}
```


2. Use the `useAuth()` hook: 

```jsx 
import useAuth from '@wasp/auth/useAuth.js'

const Page = () => {
  const { data: user, isLoading, isError } = useAuth();

  //...
}
```

You can then do things like displaying some of the user information on the page. Here's an example:


```jsx 
import useAuth from '@wasp/auth/useAuth.js'
import logout from '@wasp/auth/logout.js'
import Todo from '../Todo.js'

const Page = () => {
  const { data: user } = useAuth()

  return (
    <>
      <h1>Welcome {user.username}!</h1>
      <Todo />
      <button onClick={logout}>Logout</button>
    </>
  )
}

export default Page
```

You don't need to use the `useAuth()` hook if you're trying to protect a page from unauthorized users. Wasp takes care of that for you with its [higher-level API](/docs/language/features#authentication--authorization):

```c title="main.wasp" {28}
app Example {
  wasp: {
    version: "^0.8.0"
  },

  title: "Example",

  auth: {
    userEntity: User,
    methods: {
      usernameAndPassword: {},
    },
    onAuthFailedRedirectTo: "/login"
  }
}

// Wasp requires the userEntity to have at least the following fields
entity User {=psl
    id                        Int           @id @default(autoincrement())
    username                  String        @unique
    password                  String
psl=}

// By adding `authRequired: true` to a page, Wasp will automatically
// redirect unauthenticated users to the `onAuthFailedRedirectTo` route 
route MainRoute { path: "/", to: MainPage }
page MainPage {
  authRequired: true,
  component: import Main from "@client/MainPage"
}

route LoginRoute { path: "/login", to: LoginPage }
page LoginPage {
  component: import Login from "@client/LoginPage"
}

route SignupRoute { path: "/signup", to: SignupPage }
page SignupPage {
  component: import Signup from "@client/SignupPage"
}
```

### Server-side

To access the logged-in user server-side, you can use the `context.user` object within [Operations (i.e. *queries and actions*)](/docs/language/features#queries-and-actions-aka-operations):


```js title="src/server/actions.js" {4}
import HttpError from '@wasp/core/HttpError.js'

export const createTask = async (task, context) => {
  if (!context.user) {
    throw new HttpError(401, 'You need to be logged in to create a task.')
  }

  return context.entities.Task.create({
    data: {
      description: task.description,
      user: {
        connect: { id: context.user.id }
      }
    }
  })
}
```

