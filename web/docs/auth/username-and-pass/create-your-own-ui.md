---
title: Create your own UI
---

The login and signup flows are pretty standard: they allow the user to sign up and then log in with their username and password. The signup flow validates the username and password and then creates a new user entity in the database.

Read more about the default username and password validation rules in the [auth overview docs](../overview.md#default-validations).

## Example code

### Sign-up

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

### Login

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

## Reference

### `login()`

An action for logging in the user.

It takes two arguments:

- `username: string` <Required />

Username of the user logging in.

- `password: string` <Required />

Password of the user logging in.


:::note
When using the exposed `login()` function, make sure to implement your redirect on success login logic (e.g. redirecting to home).
:::

### `signup()`

An action for signing up the user. This action does not log in the user, you still need to call `login()`.

It takes one argument:

- `userFields: object` <Required />

  It has the following fields:

  - `username: string` <Required />

  - `password: string` <Required />

:::info
By default, Wasp will only save the `username` and `password` fields. If you want to add extra fields to your signup process, read about [defining extra signup fields](../overview.md#customizing-the-signup-process).
:::

