---
title: Create your own UI
---

The login and signup flows are pretty standard: they allow the user to sign up and then log in with their username and password. The signup flow validates the username and password and then creates a new user entity in the database.

Read more about the default username and password validation rules in the [auth overview docs](../../auth/overview.md#default-validations).

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

