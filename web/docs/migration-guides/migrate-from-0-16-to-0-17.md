## title: Migration from 0.16.X to 0.17.X

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

### Wasp no longer generates a default `favicon.ico` 

Wasp will no longer generate `favicon.ico` if there is none in the `public` folder.
Additionaly Wasp will no longer include `favicon.ico`'s `<link>` tag in `index.html` by default.

Wasp will now instead provides a default `favicon.ico` in the project's `public` folder, along with a corresponding `<link>` tag in the `main.wasp`'s `head` property.

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

### 2. Add a default `favicon.ico` to the public folder

This step is necessary only if you have no `favicon.ico` in your `public` folder.
In that case add a default `favicon.ico` to your `public` folder.

The default `favicon.ico` can be found in the output files of your Wasp project.
You can find it by going to `.wasp/out/web-app/favicon.ico` from your Wasp project's root directory.

### 3. Add a `<link>` meta tag for `favicon.ico`

This step is required for all of the project's which use `favicon.ico`.
Add the `<link>` tag to the `head` property in the `main.wasp`

```wasp title="main.wasp
app MyApp {
  // ...
  head: [
    "<link rel='icon' href='/favicon.ico' />",
  ]
}
```

### 4. Enjoy your updated Wasp app

That's it!

You should now be able to run your app with the new Wasp 0.17.0.
