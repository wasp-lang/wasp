---
title: Create your own UI
---

When using the email auth provider, users log in with their email address and a password. On signup, Wasp validates the data and sends a verification email. The user account is not active until the user clicks the link in the verification email. Also, the user can reset their password through a similar flow.

:::tip
Read more about the default email and password validation rules in the [auth overview docs](../overview.md#default-validations).
:::

Even though Wasp offers premade [Auth UI](../ui.md) for your authentication flows, there are times when you might want more customization, so we also give you the option to create your own UI and call Wasp's auth actions from your own code, similar to how Auth UI does it under the hood.

## Example code

Below you can find a starting point for making your own UI in the client code. You can customize any of its look and behaviour, just make sure to call the `signup()` or `login()` functions.

### Sign-up

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="src/pages/auth.jsx"
import { signup } from 'wasp/client/auth'

import { useState } from 'react'

export function Signup() {
  const [email, setEmail] = useState('')
  const [password, setPassword] = useState('')
  const [error, setError] = useState(null)
  const [needsConfirmation, setNeedsConfirmation] = useState(false)

  async function handleSubmit(event) {
    event.preventDefault()
    setError(null)
    try {
      await signup({ email, password })
      setNeedsConfirmation(true)
    } catch (error) {
      console.error('Error during signup:', error)
      setError(error)
    }
  }

  if (needsConfirmation) {
    return (
      <p>
        Check your email for the confirmation link. If you don't see it, check
        spam/junk folder.
      </p>
    )
  }

  return (
    <form onSubmit={handleSubmit}>
      {error && <p>Error: {error.message}</p>}

      <input
        type="email"
        value={email}
        onChange={(e) => setEmail(e.target.value)}
        placeholder="Email"
      />
      <input
        type="password"
        value={password}
        onChange={(e) => setPassword(e.target.value)}
        placeholder="Password"
      />
      <button type="submit">Sign Up</button>
    </form>
  )
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/pages/auth.tsx"
import { signup } from 'wasp/client/auth'

import { useState } from 'react'

export function Signup() {
  const [email, setEmail] = useState('')
  const [password, setPassword] = useState('')
  const [error, setError] = useState<Error | null>(null)
  const [needsConfirmation, setNeedsConfirmation] = useState(false)

  async function handleSubmit(event: React.FormEvent<HTMLFormElement>) {
    event.preventDefault()
    setError(null)
    try {
      await signup({ email, password })
      setNeedsConfirmation(true)
    } catch (error: unknown) {
      console.error('Error during signup:', error)
      setError(error as Error)
    }
  }

  if (needsConfirmation) {
    return (
      <p>
        Check your email for the confirmation link. If you don't see it, check
        spam/junk folder.
      </p>
    )
  }

  return (
    <form onSubmit={handleSubmit}>
      {error && <p>Error: {error.message}</p>}

      <input
        type="email"
        value={email}
        onChange={(e) => setEmail(e.target.value)}
        placeholder="Email"
      />
      <input
        type="password"
        value={password}
        onChange={(e) => setPassword(e.target.value)}
        placeholder="Password"
      />
      <button type="submit">Sign Up</button>
    </form>
  )
}
```

</TabItem>
</Tabs>

### Login

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="src/pages/auth.jsx"
import { login } from 'wasp/client/auth'

import { useState } from 'react'
import { useNavigate } from 'react-router-dom'

export function Login() {
  const [email, setEmail] = useState('')
  const [password, setPassword] = useState('')
  const [error, setError] = useState(null)
  const navigate = useNavigate()

  async function handleSubmit(event) {
    event.preventDefault()
    setError(null)
    try {
      await login({ email, password })
      navigate('/')
    } catch (error) {
      setError(error)
    }
  }

  return (
    <form onSubmit={handleSubmit}>
      {error && <p>Error: {error.message}</p>}

      <input
        type="email"
        value={email}
        onChange={(e) => setEmail(e.target.value)}
        placeholder="Email"
      />
      <input
        type="password"
        value={password}
        onChange={(e) => setPassword(e.target.value)}
        placeholder="Password"
      />
      <button type="submit">Log In</button>
    </form>
  )
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/pages/auth.tsx"
import { login } from 'wasp/client/auth'

import { useState } from 'react'
import { useNavigate } from 'react-router-dom'

export function Login() {
  const [email, setEmail] = useState('')
  const [password, setPassword] = useState('')
  const [error, setError] = useState<Error | null>(null)
  const navigate = useNavigate()

  async function handleSubmit(event: React.FormEvent<HTMLFormElement>) {
    event.preventDefault()
    setError(null)
    try {
      await login({ email, password })
      navigate('/')
    } catch (error: unknown) {
      setError(error as Error)
    }
  }

  return (
    <form onSubmit={handleSubmit}>
      {error && <p>Error: {error.message}</p>}

      <input
        type="email"
        value={email}
        onChange={(e) => setEmail(e.target.value)}
        placeholder="Email"
      />
      <input
        type="password"
        value={password}
        onChange={(e) => setPassword(e.target.value)}
        placeholder="Password"
      />
      <button type="submit">Log In</button>
    </form>
  )
}
```

</TabItem>
</Tabs>

## API Reference

### `login()`

An action for logging in the user.

It takes one argument:

- `userFields: object` <Required />

  It has the following fields:

  - `email: string` <Required />

  - `password: string` <Required />

:::note
When using the exposed `login()` function, make sure to implement your redirect on success login logic (e.g. redirecting to home).
:::

### `signup()`

An action for signing up the user and starting the email verification. The user will not be logged in after this, as they still need
to verify their email.

It takes one argument:

- `userFields: object` <Required />

  It has the following fields:

  - `email: string` <Required />

  - `password: string` <Required />

:::info
By default, Wasp will only save the `email` and `password` fields. If you want to add extra fields to your signup process, read about [defining extra signup fields](../overview.md#customizing-the-signup-process).
:::
