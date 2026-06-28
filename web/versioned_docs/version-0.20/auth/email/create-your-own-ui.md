---
title: Create your own UI
title-llm: Create your own UI for Email Auth
---

import { Required } from '@site/src/components/Tag'

When using the email auth provider, users log in with their email address and a password. On signup, Wasp validates the data and sends a verification email. The user account is not active until the user clicks the link in the verification email. Also, the user can reset their password through a similar flow.

:::tip
Read more about the default email and password validation rules in the [auth overview docs](../overview.md#default-validations).
:::

Even though Wasp offers premade [Auth UI](../ui.md) for your authentication flows, there are times when you might want more customization, so we also give you the option to create your own UI and call Wasp's auth actions from your own code, similar to how Auth UI does it under the hood.

## Example code

Below you can find a starting point for making your own UI in the client code. This example has all the necessary components to handle login, signup, email verification, and the password reset flow. You can customize any of its look and behaviour, just make sure to call the functions imported from `wasp/client/auth`.

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```jsx title="src/pages/auth.jsx"
    import {
      login,
      requestPasswordReset,
      resetPassword,
      signup,
      verifyEmail,
    } from 'wasp/client/auth'

    import { useState } from 'react'
    import { useNavigate } from 'react-router-dom'

    // This will be shown when the user wants to log in
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

    // This will be shown when the user wants to sign up
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

    // This will be shown has clicked on the link in their
    // email to verify their email address
    export function EmailVerification() {
      const [error, setError] = useState(null)
      const navigate = useNavigate()

      async function handleClick() {
        setError(null)
        try {
          // The token is passed as a query parameter
          const token = new URLSearchParams(window.location.search).get('token')
          if (!token) throw new Error('Token not found in URL')
          await verifyEmail({ token })
          navigate('/')
        } catch (error) {
          console.error('Error during email verification:', error)
          setError(error)
        }
      }

      return (
        <>
          {error && <p>Error: {error.message}</p>}

          <button onClick={handleClick}>Verify email</button>
        </>
      )
    }

    // This will be shown when the user wants to reset their password
    export function RequestPasswordReset() {
      const [email, setEmail] = useState('')
      const [error, setError] = useState(null)
      const [needsConfirmation, setNeedsConfirmation] = useState(false)

      async function handleSubmit(event) {
        event.preventDefault()
        setError(null)
        try {
          await requestPasswordReset({ email })
          setNeedsConfirmation(true)
        } catch (error) {
          console.error('Error during requesting reset:', error)
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

          <button type="submit">Send password reset</button>
        </form>
      )
    }

    // This will be shown when the user clicks on the link in their
    // email to reset their password
    export function PasswordReset() {
      const [error, setError] = useState(null)
      const [newPassword, setNewPassword] = useState('')
      const navigate = useNavigate()

      async function handleSubmit(event) {
        event.preventDefault()
        setError(null)
        try {
          // The token is passed as a query parameter
          const token = new URLSearchParams(window.location.search).get('token')
          if (!token) throw new Error('Token not found in URL')
          await resetPassword({ token, password: newPassword })
          navigate('/')
        } catch (error) {
          console.error('Error during password reset:', error)
          setError(error)
        }
      }

      return (
        <form onSubmit={handleSubmit}>
          {error && <p>Error: {error.message}</p>}

          <input
            type="password"
            autoComplete="new-password"
            value={newPassword}
            onChange={(e) => setNewPassword(e.target.value)}
            placeholder="New password"
          />

          <button type="submit">Reset password</button>
        </form>
      )
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```tsx title="src/pages/auth.tsx"
    import {
      login,
      requestPasswordReset,
      resetPassword,
      signup,
      verifyEmail,
    } from 'wasp/client/auth'

    import { useState } from 'react'
    import { useNavigate } from 'react-router-dom'

    // This will be shown when the user wants to log in
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

    // This will be shown when the user wants to sign up
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

    // This will be shown has clicked on the link in their
    // email to verify their email address
    export function EmailVerification() {
      const [error, setError] = useState<Error | null>(null)
      const navigate = useNavigate()

      async function handleClick() {
        setError(null)
        try {
          // The token is passed as a query parameter
          const token = new URLSearchParams(window.location.search).get('token')
          if (!token) throw new Error('Token not found in URL')
          await verifyEmail({ token })
          navigate('/')
        } catch (error: unknown) {
          console.error('Error during email verification:', error)
          setError(error as Error)
        }
      }

      return (
        <>
          {error && <p>Error: {error.message}</p>}

          <button onClick={handleClick}>Verify email</button>
        </>
      )
    }

    // This will be shown when the user wants to reset their password
    export function RequestPasswordReset() {
      const [email, setEmail] = useState('')
      const [error, setError] = useState<Error | null>(null)
      const [needsConfirmation, setNeedsConfirmation] = useState(false)

      async function handleSubmit(event: React.FormEvent<HTMLFormElement>) {
        event.preventDefault()
        setError(null)
        try {
          await requestPasswordReset({ email })
          setNeedsConfirmation(true)
        } catch (error: unknown) {
          console.error('Error during requesting reset:', error)
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

          <button type="submit">Send password reset</button>
        </form>
      )
    }

    // This will be shown when the user clicks on the link in their
    // email to reset their password
    export function PasswordReset() {
      const [error, setError] = useState<Error | null>(null)
      const [newPassword, setNewPassword] = useState('')
      const navigate = useNavigate()

      async function handleSubmit(event: React.FormEvent<HTMLFormElement>) {
        event.preventDefault()
        setError(null)
        try {
          // The token is passed as a query parameter
          const token = new URLSearchParams(window.location.search).get('token')
          if (!token) throw new Error('Token not found in URL')
          await resetPassword({ token, password: newPassword })
          navigate('/')
        } catch (error: unknown) {
          console.error('Error during password reset:', error)
          setError(error as Error)
        }
      }

      return (
        <form onSubmit={handleSubmit}>
          {error && <p>Error: {error.message}</p>}

          <input
            type="password"
            autoComplete="new-password"
            value={newPassword}
            onChange={(e) => setNewPassword(e.target.value)}
            placeholder="New password"
          />

          <button type="submit">Reset password</button>
        </form>
      )
    }
    ```
  </TabItem>
</Tabs>

## API Reference

You can import the following functions from `wasp/client/auth`:

### `login()`

An action for logging in the user. Make sure to do a redirect on success (e.g. to the main page of the app).

It takes one argument:

- `data: object` <Required />

  It has the following fields:

  - `email: string` <Required />

  - `password: string` <Required />

### `signup()`

An action for signing up the user and starting the email verification. The user will not be logged in after this, as they still need
to verify their email.

It takes one argument:

- `data: object` <Required />

  It has the following fields:

  - `email: string` <Required />

  - `password: string` <Required />

:::info
By default, Wasp will only save the `email` and `password` fields. If you want to add extra fields to your signup process, read about [defining extra signup fields](../overview.md#customizing-the-signup-process).
:::

### `verifyEmail()`

An action for marking the email as valid and the user account as active. Make sure to do a redirect on success (e.g. to the login page).

It takes one argument:

- `data: object` <Required />

  It has the following fields:

  - `token: string` <Required />

    The token that was created when signing up. It will be set as a URL Query Parameter named `token`.

### `requestPasswordReset()`

An action for asking for a password reset email. This doesn't immediately reset their password, just sends the email.

It takes one argument:

- `data: object` <Required />

  It has the following fields:

  - `email: string` <Required />

### `resetPassword()`

An action for confirming a password reset and providing the new password. Make sure to do a redirect on success (e.g. to the login page).

It takes one argument:

- `data: object` <Required />

  It has the following fields:

  - `token: string` <Required />

    The token that was created when requesting the password reset. It will be set as a URL Query Parameter named `token`.

  - `password: string` <Required />

    The new password for the user.
