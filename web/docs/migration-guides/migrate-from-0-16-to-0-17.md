---
title: Migration from 0.16.X to 0.17.X
---

## What's new in 0.17.0?

### The `login` function from `wasp/client/auth` has changed parameters

:::note
Only if you are using [username and password authentication](../auth/username-and-pass.md).
:::

The `login` function, as imported from `wasp/client/auth`, has changed
the way of calling it:

- ❌ **OLD**: `login(usernameValue, passwordValue)`
- ✅ **NEW**: `login({ username: usernameValue, password: passwordValue })`

This is to make it consistent with the `login` and `signup` calls in other
authentication methods, which were already using this convention.

## How to migrate?

To migrate your Wasp app from 0.16.X to 0.17.X, follow these steps:

### 1. Change the parameters to the `login` function

:::note
Only if you are using [username and password authentication](../auth/username-and-pass.md).
:::

If you were using the `login` function (imported from `wasp/client/auth`),
change its parameters from `login(usernameValue, passwordValue)` to
`login({ username: usernameValue, password: passwordValue })`.

It is possible that you were not using this function in your code.
If you're instead using [the `<LoginForm>` component](../auth/ui.md#login-form),
this change is already handled for you.

### 2. Enjoy your updated Wasp app

That's it!

You should now be able to run your app with the new Wasp 0.17.0.
