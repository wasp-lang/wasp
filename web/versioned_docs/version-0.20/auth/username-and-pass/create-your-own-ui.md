---
title: Create your own UI
title-llm: Create your own UI for Username & Password Auth
---

import { Required } from '@site/src/components/Tag'

The login and signup flows are pretty standard: they allow the user to sign up and then log in with their username and password. The signup flow validates the username and password and then creates a new user entity in the database.

:::tip
Read more about the default email and password validation rules in the [auth overview docs](../overview.md#default-validations).
:::

Even though Wasp offers premade [Auth UI](../ui.md) for your authentication flows, there are times where you might want more customization, so we also give you the option to create your own UI and call Wasp's auth actions on your own code, similar to how Auth UI does it under the hood.

## Example code

Below you can find a starting point for making your own UI in the client code. You can customize any of its look and behaviour, just make sure to call the `signup()` or `login()` functions imported from `wasp/client/auth`.

### Sign-up

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```jsx title="src/pages/auth.jsx"
    import { login, signup } from 'wasp/client/auth'

    import { useState } from 'react'
    import { useNavigate } from 'react-router-dom'

    export function Signup() {
      const [username, setUsername] = useState('')
      const [password, setPassword] = useState('')
      const [error, setError] = useState(null)
      const navigate = useNavigate()

      async function handleSubmit(event) {
        event.preventDefault()
        setError(null)
        try {
          await signup({ username, password })
          await login({ username, password })
          navigate('/')
        } catch (error) {
          setError(error)
        }
      }

      return (
        <form onSubmit={handleSubmit}>
          {error && <p>Error: {error.message}</p>}

          <input
            type="text"
            autoComplete="username"
            value={username}
            onChange={(e) => setUsername(e.target.value)}
            placeholder="Username"
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
    import { login, signup } from 'wasp/client/auth'

    import { useState } from 'react'
    import { useNavigate } from 'react-router-dom'

    export function Signup() {
      const [username, setUsername] = useState('')
      const [password, setPassword] = useState('')
      const [error, setError] = useState<Error | null>(null)
      const navigate = useNavigate()

      async function handleSubmit(event: React.FormEvent<HTMLFormElement>) {
        event.preventDefault()
        setError(null)
        try {
          await signup({ username, password })
          await login({ username, password })
          navigate('/')
        } catch (error: unknown) {
          setError(error as Error)
        }
      }

      return (
        <form onSubmit={handleSubmit}>
          {error && <p>Error: {error.message}</p>}

          <input
            type="text"
            autoComplete="username"
            value={username}
            onChange={(e) => setUsername(e.target.value)}
            placeholder="Username"
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
      const [username, setUsername] = useState('')
      const [password, setPassword] = useState('')
      const [error, setError] = useState(null)
      const navigate = useNavigate()

      async function handleSubmit(event) {
        event.preventDefault()
        setError(null)
        try {
          await login({ username, password })
          navigate('/')
        } catch (error) {
          setError(error)
        }
      }

      return (
        <form onSubmit={handleSubmit}>
          {error && <p>Error: {error.message}</p>}

          <input
            type="text"
            autoComplete="username"
            value={username}
            onChange={(e) => setUsername(e.target.value)}
            placeholder="Username"
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
    import { login } from 'wasp/client/auth'

    import { useState } from 'react'
    import { useNavigate } from 'react-router-dom'

    export function Login() {
      const [username, setUsername] = useState('')
      const [password, setPassword] = useState('')
      const [error, setError] = useState<Error | null>(null)
      const navigate = useNavigate()

      async function handleSubmit(event: React.FormEvent<HTMLFormElement>) {
        event.preventDefault()
        setError(null)
        try {
          await login({ username, password })
          navigate('/')
        } catch (error: unknown) {
          setError(error as Error)
        }
      }

      return (
        <form onSubmit={handleSubmit}>
          {error && <p>Error: {error.message}</p>}

          <input
            type="text"
            autoComplete="username"
            value={username}
            onChange={(e) => setUsername(e.target.value)}
            placeholder="Username"
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

## API Reference

You can import the following functions from `wasp/client/auth`:

### `login()`

An action for logging in the user.

It takes one argument:

- `data: object` <Required />

  It has the following fields:

  - `username: string` <Required />

  - `password: string` <Required />

:::note
When using the exposed `login()` function, make sure to implement your redirect on success login logic (e.g. redirecting to home).
:::

### `signup()`

An action for signing up the user. This action does not log in the user, you still need to call `login()`.

It takes one argument:

- `data: object` <Required />

  It has the following fields:

  - `username: string` <Required />

  - `password: string` <Required />

:::info
By default, Wasp will only save the `username` and `password` fields. If you want to add extra fields to your signup process, read about [defining extra signup fields](../overview.md#customizing-the-signup-process).
:::
