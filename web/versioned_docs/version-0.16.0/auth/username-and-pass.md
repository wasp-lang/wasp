---
title: Username & Password
---

import { Required } from '@site/src/components/Tag';
import MultipleIdentitiesWarning from './\_multiple-identities-warning.md';
import ReadMoreAboutAuthEntities from './\_read-more-about-auth-entities.md';
import UserSignupFieldsExplainer from './\_user-signup-fields-explainer.md';
import UserFieldsExplainer from './\_user-fields.md';
import UsernameData from './entities/\_username-data.md';
import AccessingUserDataNote from './\_accessing-user-data-note.md';

Wasp supports username & password authentication out of the box with login and signup flows. It provides you with the server-side implementation and the UI components for the client side.

## Setting Up Username & Password Authentication

To set up username authentication we need to:

1. Enable username authentication in the Wasp file
1. Add the `User` entity
1. Add the auth routes and pages
1. Use Auth UI components in our pages

Structure of the `main.wasp` file we will end up with:

```wasp title="main.wasp"
// Configuring e-mail authentication
app myApp {
  auth: { ... }
}

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
    version: "^0.15.0"
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
    version: "^0.15.0"
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

The `User` entity can be as simple as including only the `id` field:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```prisma title="schema.prisma"
// 3. Define the user entity
model User {
  // highlight-next-line
  id Int @id @default(autoincrement())
  // Add your own fields below
  // ...
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```prisma title="schema.prisma"
// 3. Define the user entity
model User {
  // highlight-next-line
  id Int @id @default(autoincrement())
  // Add your own fields below
  // ...
}
```

</TabItem>
</Tabs>

<ReadMoreAboutAuthEntities />

### 3. Add the Routes and Pages

Next, we need to define the routes and pages for the authentication pages.

Add the following to the `main.wasp` file:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...
route LoginRoute { path: "/login", to: LoginPage }
page LoginPage {
  component: import { Login } from "@src/pages/auth.jsx"
}
route SignupRoute { path: "/signup", to: SignupPage }
page SignupPage {
  component: import { Signup } from "@src/pages/auth.jsx"
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...
route LoginRoute { path: "/login", to: LoginPage }
page LoginPage {
  component: import { Login } from "@src/pages/auth.tsx"
}
route SignupRoute { path: "/signup", to: SignupPage }
page SignupPage {
  component: import { Signup } from "@src/pages/auth.tsx"
}
```

</TabItem>
</Tabs>

We'll define the React components for these pages in the `src/pages/auth.{jsx,tsx}` file below.

### 4. Create the Client Pages

:::info
We are using [Tailwind CSS](https://tailwindcss.com/) to style the pages. Read more about how to add it [here](../project/css-frameworks).
:::

Let's create a `auth.{jsx,tsx}` file in the `src/pages` folder and add the following to it:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```tsx title="src/pages/auth.jsx"
import { LoginForm, SignupForm } from 'wasp/client/auth'
import { Link } from 'react-router-dom'

export function Login() {
  return (
    <Layout>
      <LoginForm />
      <br />
      <span className="text-sm font-medium text-gray-900">
        Don't have an account yet? <Link to="/signup">go to signup</Link>.
      </span>
    </Layout>
  )
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
  )
}

// A layout component to center the content
export function Layout({ children }) {
  return (
    <div className="h-full w-full bg-white">
      <div className="flex min-h-[75vh] min-w-full items-center justify-center">
        <div className="h-full w-full max-w-sm bg-white p-5">
          <div>{children}</div>
        </div>
      </div>
    </div>
  )
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/pages/auth.tsx"
import { LoginForm, SignupForm } from 'wasp/client/auth'
import { Link } from 'react-router-dom'

export function Login() {
  return (
    <Layout>
      <LoginForm />
      <br />
      <span className="text-sm font-medium text-gray-900">
        Don't have an account yet? <Link to="/signup">go to signup</Link>.
      </span>
    </Layout>
  )
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
  )
}

// A layout component to center the content
export function Layout({ children }: { children: React.ReactNode }) {
  return (
    <div className="h-full w-full bg-white">
      <div className="flex min-h-[75vh] min-w-full items-center justify-center">
        <div className="h-full w-full max-w-sm bg-white p-5">
          <div>{children}</div>
        </div>
      </div>
    </div>
  )
}
```

</TabItem>
</Tabs>

We imported the generated Auth UI components and used them in our pages. Read more about the Auth UI components [here](../auth/ui).

### Conclusion

That's it! We have set up username authentication in our app. 🎉

Running `wasp db migrate-dev` and then `wasp start` should give you a working app with username authentication. If you want to put some of the pages behind authentication, read the [auth overview docs](../auth/overview).

<MultipleIdentitiesWarning />

## Customizing the Auth Flow

The login and signup flows are pretty standard: they allow the user to sign up and then log in with their username and password. The signup flow validates the username and password and then creates a new user entity in the database.

Read more about the default username and password validation rules in the [auth overview docs](../auth/overview#default-validations).

If you require more control in your authentication flow, you can achieve that in the following ways:

1. Create your UI and use `signup` and `login` actions.
1. Create your custom sign-up action which uses the lower-level API, along with your custom code.

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

```jsx title="src/pages/auth.jsx"
import { login } from 'wasp/client/auth'

import { useState } from 'react'
import { useNavigate, Link } from 'react-router-dom'

export function LoginPage() {
  const [username, setUsername] = useState('')
  const [password, setPassword] = useState('')
  const [error, setError] = useState(null)
  const navigate = useNavigate()

  async function handleSubmit(event) {
    event.preventDefault()
    try {
      await login(username, password)
      navigate('/')
    } catch (error) {
      setError(error)
    }
  }

  return <form onSubmit={handleSubmit}>{/* ... */}</form>
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/pages/auth.tsx"
import { login } from 'wasp/client/auth'

import { useState } from 'react'
import { useNavigate, Link } from 'react-router-dom'

export function LoginPage() {
  const [username, setUsername] = useState('')
  const [password, setPassword] = useState('')
  const [error, setError] = useState<Error | null>(null)
  const navigate = useNavigate()

  async function handleSubmit(event: React.FormEvent<HTMLFormElement>) {
    event.preventDefault()
    try {
      await login(username, password)
      navigate('/')
    } catch (error: unknown) {
      setError(error as Error)
    }
  }

  return <form onSubmit={handleSubmit}>{/* ... */}</form>
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
  By default, Wasp will only save the `username` and `password` fields. If you want to add extra fields to your signup process, read about [defining extra signup fields](../auth/overview#customizing-the-signup-process).
  :::

You can use it like this:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="src/pages/auth.jsx"
import { signup, login } from 'wasp/client/auth'

import { useState } from 'react'
import { useNavigate, Link } from 'react-router-dom'

export function Signup() {
  const [username, setUsername] = useState('')
  const [password, setPassword] = useState('')
  const [error, setError] = useState(null)
  const navigate = useNavigate()

  async function handleSubmit(event) {
    event.preventDefault()
    try {
      await signup({
        username,
        password,
      })
      await login(username, password)
      navigate('/')
    } catch (error) {
      setError(error)
    }
  }

  return <form onSubmit={handleSubmit}>{/* ... */}</form>
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/pages/auth.tsx"
import { signup, login } from 'wasp/client/auth'

import { useState } from 'react'
import { useNavigate, Link } from 'react-router-dom'

export function Signup() {
  const [username, setUsername] = useState('')
  const [password, setPassword] = useState('')
  const [error, setError] = useState<Error | null>(null)
  const navigate = useNavigate()

  async function handleSubmit(event: React.FormEvent<HTMLFormElement>) {
    event.preventDefault()
    try {
      await signup({
        username,
        password,
      })
      await login(username, password)
      navigate('/')
    } catch (error: unknown) {
      setError(error as Error)
    }
  }

  return <form onSubmit={handleSubmit}>{/* ... */}</form>
}
```

</TabItem>
</Tabs>

### 2. Creating your custom sign-up action

The code of your custom sign-up action can look like this:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...

action customSignup {
  fn: import { signup } from "@src/auth/signup.js",
}
```

```js title="src/auth/signup.js"
import {
  ensurePasswordIsPresent,
  ensureValidPassword,
  ensureValidUsername,
  createProviderId,
  sanitizeAndSerializeProviderData,
  createUser,
} from 'wasp/server/auth'

export const signup = async (args, _context) => {
  ensureValidUsername(args)
  ensurePasswordIsPresent(args)
  ensureValidPassword(args)

  try {
    const providerId = createProviderId('username', args.username)
    const providerData = await sanitizeAndSerializeProviderData({
      hashedPassword: args.password,
    })

    await createUser(
      providerId,
      providerData,
      // Any additional data you want to store on the User entity
      {}
    )
  } catch (e) {
    return {
      success: false,
      message: e.message,
    }
  }

  // Your custom code after sign-up.
  // ...

  return {
    success: true,
    message: 'User created successfully',
  }
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

action customSignup {
  fn: import { signup } from "@src/auth/signup",
}
```

```ts title="src/auth/signup.ts"
import {
  ensurePasswordIsPresent,
  ensureValidPassword,
  ensureValidUsername,
  createProviderId,
  sanitizeAndSerializeProviderData,
  createUser,
} from 'wasp/server/auth'
import type { CustomSignup } from 'wasp/server/operations'

type CustomSignupInput = {
  username: string
  password: string
}
type CustomSignupOutput = {
  success: boolean
  message: string
}

export const signup: CustomSignup<
  CustomSignupInput,
  CustomSignupOutput
> = async (args, _context) => {
  ensureValidUsername(args)
  ensurePasswordIsPresent(args)
  ensureValidPassword(args)

  try {
    const providerId = createProviderId('username', args.username)
    const providerData = await sanitizeAndSerializeProviderData<'username'>({
      hashedPassword: args.password,
    })

    await createUser(
      providerId,
      providerData,
      // Any additional data you want to store on the User entity
      {}
    )
  } catch (e) {
    return {
      success: false,
      message: e.message,
    }
  }

  // Your custom code after sign-up.
  // ...

  return {
    success: true,
    message: 'User created successfully',
  }
}
```

</TabItem>
</Tabs>

We suggest using the built-in field validators for your authentication flow. You can import them from `wasp/server/auth`. These are the same validators that Wasp uses internally for the default authentication flow.

#### Username

- `ensureValidUsername(args)`

  Checks if the username is valid and throws an error if it's not. Read more about the validation rules [here](../auth/overview#default-validations).

#### Password

- `ensurePasswordIsPresent(args)`

  Checks if the password is present and throws an error if it's not.

- `ensureValidPassword(args)`

  Checks if the password is valid and throws an error if it's not. Read more about the validation rules [here](../auth/overview#default-validations).

## Using Auth

To read more about how to set up the logout button and how to get access to the logged-in user in our client and server code, read the [auth overview docs](../auth/overview).

When you receive the `user` object [on the client or the server](./overview.md#accessing-the-logged-in-user), you'll be able to access the user's username like this:

<UsernameData />

<AccessingUserDataNote />

## API Reference

### `userEntity` fields

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.15.0"
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
```

```prisma title="schema.prisma"
model User {
  id Int @id @default(autoincrement())
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.15.0"
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
```

```prisma title="schema.prisma"
model User {
  id Int @id @default(autoincrement())
}
```

</TabItem>
</Tabs>

<UserFieldsExplainer />

### Fields in the `usernameAndPassword` dict

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.15.0"
  },
  title: "My App",
  auth: {
    userEntity: User,
    methods: {
      usernameAndPassword: {
        userSignupFields: import { userSignupFields } from "@src/auth/email.js",
      },
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
    version: "^0.15.0"
  },
  title: "My App",
  auth: {
    userEntity: User,
    methods: {
      usernameAndPassword: {
        userSignupFields: import { userSignupFields } from "@src/auth/email.js",
      },
    },
    onAuthFailedRedirectTo: "/login"
  }
}
// ...
```

</TabItem>
</Tabs>

#### `userSignupFields: ExtImport`

<UserSignupFieldsExplainer />
Read more about the `userSignupFields` function [here](./overview#1-defining-extra-fields).
