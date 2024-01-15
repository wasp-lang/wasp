---
title: Username & Password
---

import { Required } from '@site/src/components/Tag';

Wasp supports username & password authentication out of the box with login and signup flows. It provides you with the server-side implementation and the UI components for the client-side.

## Setting Up Username & Password Authentication

To set up username authentication we need to:
1. Enable username authentication in the Wasp file
1. Add the user entity
1. Add the routes and pages
1. Use Auth UI components in our pages

Structure of the `main.wasp` file we will end up with:

```wasp title="main.wasp"
// Configuring e-mail authentication
app myApp {
  auth: { ... }
}
// Defining User entity
entity User { ... }
// Defining routes and pages
route SignupRoute { ... }
page SignupPage { ... }
// ...
```

### 1. Enable Username Authentication

Let's start with adding the following to our `main.wasp` file:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp" {11}
app myApp {
  wasp: {
    version: "^0.11.0"
  },
  title: "My App",
  auth: {
    // 1. Specify the user entity (we'll define it next)
    userEntity: User,
    methods: {
      // 2. Enable username authentication
      usernameAndPassword: {},
    },
    onAuthFailedRedirectTo: "/login"
  }
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"  {11}
app myApp {
  wasp: {
    version: "^0.11.0"
  },
  title: "My App",
  auth: {
    // 1. Specify the user entity (we'll define it next)
    userEntity: User,
    methods: {
      // 2. Enable username authentication
      usernameAndPassword: {},
    },
    onAuthFailedRedirectTo: "/login"
  }
}
```
</TabItem>
</Tabs>

Read more about the `usernameAndPassword` auth method options [here](#fields-in-the-usernameandpassword-dict).

### 2. Add the User Entity

When username authentication is enabled, Wasp expects certain fields in your `userEntity`. Let's add these fields to our `main.wasp` file:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp" {4-5}
// 3. Define the user entity
entity User {=psl
    id                        Int           @id @default(autoincrement())
    username                  String        @unique
    password                  String
    // Add your own fields below
    // ...
psl=}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp" {4-5}
// 3. Define the user entity
entity User {=psl
    id                        Int           @id @default(autoincrement())
    username                  String        @unique
    password                  String
    // Add your own fields below
    // ...
psl=}
```
</TabItem>
</Tabs>

Read more about the `userEntity` fields [here](#userentity-fields).

### 3. Add the Routes and Pages

Next, we need to define the routes and pages for the authentication pages.

Add the following to the `main.wasp` file:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...
// 4. Define the routes
route LoginRoute { path: "/login", to: LoginPage }
page LoginPage {
  component: import { Login } from "@client/pages/auth.jsx"
}
route SignupRoute { path: "/signup", to: SignupPage }
page SignupPage {
  component: import { Signup } from "@client/pages/auth.jsx"
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...
// 4. Define the routes
route LoginRoute { path: "/login", to: LoginPage }
page LoginPage {
  component: import { Login } from "@client/pages/auth.tsx"
}
route SignupRoute { path: "/signup", to: SignupPage }
page SignupPage {
  component: import { Signup } from "@client/pages/auth.tsx"
}
```
</TabItem>
</Tabs>

We'll define the React components for these pages in the `client/pages/auth.{jsx,tsx}` file below.

### 4. Create the Client Pages

:::info
We are using [Tailwind CSS](https://tailwindcss.com/) to style the pages. Read more about how to add it [here](../project/css-frameworks).
:::

Let's create a `auth.{jsx,tsx}` file in the `client/pages` folder and add the following to it:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```tsx title="client/pages/auth.jsx"
import { LoginForm } from "@wasp/auth/forms/Login";
import { SignupForm } from "@wasp/auth/forms/Signup";
import { Link } from "react-router-dom";

export function Login() {
  return (
    <Layout>
      <LoginForm />
      <br />
      <span className="text-sm font-medium text-gray-900">
        Don't have an account yet? <Link to="/signup">go to signup</Link>.
      </span>
    </Layout>
  );
}

export function Signup() {
  return (
    <Layout>
      <SignupForm />
      <br />
      <span className="text-sm font-medium text-gray-900">
        I already have an account (<Link to="/login">go to login</Link>).
      </span>
    </Layout>
  );
}

// A layout component to center the content
export function Layout({ children }) {
  return (
    <div className="w-full h-full bg-white">
      <div className="min-w-full min-h-[75vh] flex items-center justify-center">
        <div className="w-full h-full max-w-sm p-5 bg-white">
          <div>{children}</div>
        </div>
      </div>
    </div>
  );
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="client/pages/auth.tsx"
import { LoginForm } from "@wasp/auth/forms/Login";
import { SignupForm } from "@wasp/auth/forms/Signup";
import { Link } from "react-router-dom";

export function Login() {
  return (
    <Layout>
      <LoginForm />
      <br />
      <span className="text-sm font-medium text-gray-900">
        Don't have an account yet? <Link to="/signup">go to signup</Link>.
      </span>
    </Layout>
  );
}

export function Signup() {
  return (
    <Layout>
      <SignupForm />
      <br />
      <span className="text-sm font-medium text-gray-900">
        I already have an account (<Link to="/login">go to login</Link>).
      </span>
    </Layout>
  );
}

// A layout component to center the content
export function Layout({ children }: { children: React.ReactNode }) {
  return (
    <div className="w-full h-full bg-white">
      <div className="min-w-full min-h-[75vh] flex items-center justify-center">
        <div className="w-full h-full max-w-sm p-5 bg-white">
          <div>{children}</div>
        </div>
      </div>
    </div>
  );
}
```
</TabItem>
</Tabs>

We imported the generated Auth UI components and used them in our pages. Read more about the Auth UI components [here](../auth/ui).

### Conclusion

That's it! We have set up username authentication in our app. 🎉

Running `wasp db migrate-dev` and then `wasp start` should give you a working app with username authentication. If you want to put some of the pages behind authentication, read the [using auth docs](../auth/overview).

## Customizing the Auth Flow

The login and signup flows are pretty standard: they allow the user to sign up and then log in with their username and password. The signup flow validates the username and password and then creates a new user entity in the database.

Read more about the default username and password validation rules and how to override them in the [using auth docs](../auth/overview).

If you require more control in your authentication flow, you can achieve that in the following ways:
1. Create your UI and use `signup` and `login` actions.
1. Create your custom sign-up and login [actions](#) which uses the Prisma client, along with your custom code.

### 1. Using the `signup` and `login` actions

#### `login()`
An action for logging in the user.

It takes two arguments:

  - `username: string` <Required />

  Username of the user logging in.

  - `password: string` <Required />

  Password of the user logging in.

You can use it like this:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="client/pages/auth.jsx"
// Importing the login action 👇
import login from '@wasp/auth/login'

import { useState } from 'react'
import { useHistory } from 'react-router-dom'
import { Link } from 'react-router-dom'

export function LoginPage() {
  const [username, setUsername] = useState('')
  const [password, setPassword] = useState('')
  const [error, setError] = useState(null)
  const history = useHistory()

  async function handleSubmit(event) {
    event.preventDefault()
    try {
      await login(username, password)
      history.push('/')
    } catch (error) {
      setError(error)
    }
  }

  return (
    <form onSubmit={handleSubmit}>
      {/* ... */}
    </form>
  );
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="client/pages/auth.tsx"
// Importing the login action 👇
import login from '@wasp/auth/login'

import { useState } from 'react'
import { useHistory } from 'react-router-dom'
import { Link } from 'react-router-dom'

export function LoginPage() {
  const [username, setUsername] = useState('')
  const [password, setPassword] = useState('')
  const [error, setError] = useState<Error | null>(null)
  const history = useHistory()

  async function handleSubmit(event: React.FormEvent<HTMLFormElement>) {
    event.preventDefault()
    try {
      await login(username, password)
      history.push('/')
    } catch (error: unknown) {
      setError(error as Error)
    }
  }

  return (
    <form onSubmit={handleSubmit}>
      {/* ... */}
    </form>
  );
}
```
</TabItem>
</Tabs>

:::note
When using the exposed `login()` function, make sure to implement your redirect on success login logic (e.g. redirecting to home).
:::

#### `signup()`
An action for signing up the user. This action does not log in the user, you still need to call `login()`.

It takes one argument:
- `userFields: object` <Required />

  It has the following fields:
  - `username: string` <Required />

  - `password: string` <Required />

  :::info
  Wasp only stores the auth-related fields of the user entity. Adding extra fields to `userFields` will not have any effect.

  If you need to add extra fields to the user entity, we suggest doing it in a separate step after the user logs in for the first time.
  :::

You can use it like this:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="client/pages/auth.jsx"
// Importing the signup and login actions 👇
import signup from '@wasp/auth/signup'
import login from '@wasp/auth/login'

import { useState } from 'react'
import { useHistory } from 'react-router-dom'
import { Link } from 'react-router-dom'

export function Signup() {
  const [username, setUsername] = useState('')
  const [password, setPassword] = useState('')
  const [error, setError] = useState(null)
  const history = useHistory()

  async function handleSubmit(event) {
    event.preventDefault()
    try {
      await signup({
        username,
        password,
      })
      await login(username, password)
      history.push("/")
    } catch (error) {
      setError(error)
    }
  }

  return (
    <form onSubmit={handleSubmit}>
      {/* ... */}
    </form>
  );
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="client/pages/auth.tsx"
// Importing the signup and login actions 👇
import signup from '@wasp/auth/signup'
import login from '@wasp/auth/login'

import { useState } from 'react'
import { useHistory } from 'react-router-dom'
import { Link } from 'react-router-dom'

export function Signup() {
  const [username, setUsername] = useState('')
  const [password, setPassword] = useState('')
  const [error, setError] = useState<Error | null>(null)
  const history = useHistory()

  async function handleSubmit(event: React.FormEvent<HTMLFormElement>) {
    event.preventDefault()
    try {
      await signup({
        username,
        password,
      })
      await login(username, password)
      history.push("/")
    } catch (error: unknown) {
      setError(error as Error)
    }
  }

  return (
    <form onSubmit={handleSubmit}>
      {/* ... */}
    </form>
  );
}
```
</TabItem>
</Tabs>

### 2. Creating your custom actions

The code of your custom sign-up action can look like this:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...

action signupUser {
  fn: import { signUp } from "@server/auth/signup.js",
  entities: [User]
}
```


```js title="src/server/auth/signup.js"
export const signUp = async (args, context) => {
  // Your custom code before sign-up.
  // ...

  const newUser = context.entities.User.create({
    data: {
      username: args.username,
      password: args.password // password hashed automatically by Wasp! 🐝
    }
  })

  // Your custom code after sign-up.
  // ...
  return newUser
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

action signupUser {
  fn: import { signUp } from "@server/auth/signup.js",
  entities: [User]
}
```

```ts title="src/server/auth/signup.ts"
import type { User } from '@wasp/entities'
import type { SignupUser } from '@wasp/actions/types'

type SignupPayload = Pick<User, 'username' | 'password'>

export const signUp: SignupUser<SignupPayload, User> = async (args, context) => {
  // Your custom code before sign-up.
  // ...

  const newUser = context.entities.User.create({
    data: {
      username: args.username,
      password: args.password // password hashed automatically by Wasp! 🐝
    }
  })

  // Your custom code after sign-up.
  // ...
  return newUser
}
```
</TabItem>
</Tabs>

## Using Auth 

To read more about how to set up the logout button and how to get access to the logged-in user in our client and server code, read the [using auth docs](../auth/overview).

## API Reference

### `userEntity` fields

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.11.0"
  },
  title: "My App",
  auth: {
    userEntity: User,
    methods: {
      usernameAndPassword: {},
    },
    onAuthFailedRedirectTo: "/login"
  }
}

// Wasp requires the `userEntity` to have at least the following fields
entity User {=psl
    id                        Int           @id @default(autoincrement())
    username                  String        @unique
    password                  String
psl=}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.11.0"
  },
  title: "My App",
  auth: {
    userEntity: User,
    methods: {
      usernameAndPassword: {},
    },
    onAuthFailedRedirectTo: "/login"
  }
}

// Wasp requires the `userEntity` to have at least the following fields
entity User {=psl
    id                        Int           @id @default(autoincrement())
    username                  String        @unique
    password                  String
psl=}
```
</TabItem>
</Tabs>

Username & password auth requires that `userEntity` specified in `auth` contains:

- `username` field of type `String`
- `password` field of type `String`

### Fields in the `usernameAndPassword` dict

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.11.0"
  },
  title: "My App",
  auth: {
    userEntity: User,
    methods: {
      usernameAndPassword: {},
    },
    onAuthFailedRedirectTo: "/login"
  }
}
// ...
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.11.0"
  },
  title: "My App",
  auth: {
    userEntity: User,
    methods: {
      usernameAndPassword: {},
    },
    onAuthFailedRedirectTo: "/login"
  }
}
// ...
```
</TabItem>
</Tabs>

:::info 
`usernameAndPassword` dict doesn't have any options at the moment.
:::

You can read about the rest of the `auth` options in the [using auth](../auth/overview) section of the docs.
