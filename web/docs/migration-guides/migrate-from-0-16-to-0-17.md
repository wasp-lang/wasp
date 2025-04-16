---
title: Migration from 0.16.X to 0.17.X
---

## What's new in 0.17.0?

### The `login` function parameters changed (username & password only)

:::info
This change only affects you if you're using [username and password authentication](../auth/username-and-pass.md) with
[custom auth UI](../auth/username-and-pass/create-your-own-ui.md). If you're using [email authentication](../auth/email.md),
[social authentication](../auth/social-auth/overview.md), or our premade [Auth UI](../auth/ui.md) components,
you don't need to take any action.
:::

The `login` function, as imported from `wasp/client/auth`, has changed
the way of calling it:

<Tabs>
<TabItem value="before" label="Before">

```ts
import { login } from 'wasp/client/auth'

await login(usernameValue, passwordValue)
```

</TabItem>
<TabItem value="after" label="After">

```ts
import { login } from 'wasp/client/auth'

await login({ username: usernameValue, password: passwordValue })
```

</TabItem>
</Tabs>

This is to make it consistent with the `login` and `signup` calls in other
authentication methods, which were already using this convention.

## How to migrate?

To migrate your Wasp app from 0.16.X to 0.17.X, follow these steps:

### 1. Change the parameters to the `login` function (username & password only)

:::info
This change only affects you if you're using [username and password authentication](../auth/username-and-pass.md) with
[custom auth UI](../auth/username-and-pass/create-your-own-ui.md). If you're using [email authentication](../auth/email.md),
[social authentication](../auth/social-auth/overview.md), or our premade [Auth UI](../auth/ui.md) components,
you don't need to take any action.
:::

If you were using the `login` function (imported from `wasp/client/auth`),
change its parameters from `login(usernameValue, passwordValue)` to
`login({ username: usernameValue, password: passwordValue })`.

<Tabs>
<TabItem value="before" label="Before">

```tsx title="src/components/MyLoginForm.tsx"
import { login } from 'wasp/client/auth'

export const MyLoginForm = () => {
  const [usernameValue, setUsernameValue] = useState('')
  const [passwordValue, setPasswordValue] = useState('')

  const handleSubmit = async (e) => {
    e.preventDefault()
    await login(usernameValue, passwordValue)
    // ...
  }

  return <form onSubmit={handleSubmit}>{/* ... */}</form>
}
```

</TabItem>
<TabItem value="after" label="After">

```tsx title="src/components/MyLoginForm.tsx"
import { login } from 'wasp/client/auth'

export const MyLoginForm = () => {
  const [usernameValue, setUsernameValue] = useState('')
  const [passwordValue, setPasswordValue] = useState('')

  const handleSubmit = async (e) => {
    e.preventDefault()
    await login({ username: usernameValue, password: passwordValue })
    // ...
  }

  return <form onSubmit={handleSubmit}>{/* ... */}</form>
}
```

</TabItem>
</Tabs>

It is possible that you were not using this function in your code.
If you're instead using [the `<LoginForm>` component](../auth/ui.md#login-form),
this change is already handled for you.

### 2. Enjoy your updated Wasp app

That's it!

You should now be able to run your app with the new Wasp 0.17.0.
