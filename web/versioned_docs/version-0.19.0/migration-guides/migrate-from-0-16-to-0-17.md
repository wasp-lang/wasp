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
import { login } from "wasp/client/auth";

await login(usernameValue, passwordValue);
```

</TabItem>
<TabItem value="after" label="After">

```ts
import { login } from "wasp/client/auth";

await login({ username: usernameValue, password: passwordValue });
```

</TabItem>
</Tabs>

This is to make it consistent with the `login` and `signup` calls in other
authentication methods, which were already using this convention.

### Wasp no longer generates a default `favicon.ico`

Wasp will no longer generate `favicon.ico` if there isn't one in the `public` directory.
Also, Wasp will no longer generate a `<link>` meta tag in `index.html`. You'll need to define it yourself explicitly.

New Wasp projects come with a default `favicon.ico` in the `public` directory and the `<link>` meta tag in the `main.wasp` file.

## How to migrate?

To migrate your Wasp app from 0.16.X to 0.17.X, follow these steps:

### 1. Bump the Wasp version

Update the version field in your Wasp file to `^0.17.0`:

```wasp title="main.wasp"
app MyApp {
  wasp: {
    // highlight-next-line
    version: "^0.17.0"
  },
}
```

### 2. Change the parameters to the `login` function (username & password only)

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
import { login } from "wasp/client/auth";

export const MyLoginForm = () => {
  const [usernameValue, setUsernameValue] = useState("");
  const [passwordValue, setPasswordValue] = useState("");

  const handleSubmit = async (e) => {
    e.preventDefault();
    await login(usernameValue, passwordValue);
    // ...
  };

  return <form onSubmit={handleSubmit}>{/* ... */}</form>;
};
```

</TabItem>
<TabItem value="after" label="After">

```tsx title="src/components/MyLoginForm.tsx"
import { login } from "wasp/client/auth";

export const MyLoginForm = () => {
  const [usernameValue, setUsernameValue] = useState("");
  const [passwordValue, setPasswordValue] = useState("");

  const handleSubmit = async (e) => {
    e.preventDefault();
    await login({ username: usernameValue, password: passwordValue });
    // ...
  };

  return <form onSubmit={handleSubmit}>{/* ... */}</form>;
};
```

</TabItem>
</Tabs>

It is possible that you were not using this function in your code.
If you're instead using [the `<LoginForm>` component](../auth/ui.md#login-form),
this change is already handled for you.

### 3. Update your `tsconfig.json`

To ensure your project works correctly with Wasp 0.17.0, you must also update your
`tsconfig.json` file.

If you haven't changed anything in your project's `tsconfig.json` file (this is
the case for most users), just replace its contents with the new version shown
below.

If you have made changes to your `tsconfig.json` file, we recommend taking the
new version of the file and reapplying them.

Here's the new version of `tsconfig.json`:

```json title="tsconfig.json"
// =============================== IMPORTANT =================================
// This file is mainly used for Wasp IDE support.
//
// Wasp will compile your code with slightly different (less strict) compilerOptions.
// You can increase the configuration's strictness (e.g., by adding
// "noUncheckedIndexedAccess": true), but you shouldn't reduce it (e.g., by
// adding "strict": false). Just keep in mind that this will only affect your
// IDE support, not the actual compilation.
//
// Full TypeScript configurability is coming very soon :)
{
  "compilerOptions": {
    "module": "esnext",
    "composite": true,
    "target": "esnext",
    "moduleResolution": "bundler",
    "jsx": "preserve",
    "strict": true,
    "esModuleInterop": true,
    "isolatedModules": true,
    "moduleDetection": "force",
    "lib": ["dom", "dom.iterable", "esnext"],
    "skipLibCheck": true,
    "allowJs": true,
    "outDir": ".wasp/out/user"
  },
  "include": ["src"]
}
```

### 4. Update your `package.json`

Wasp now requires `typescript` to be set to version `5.8.2`.

Here’s the updated `package.json` snippet:

<Tabs>
<TabItem value="before" label="Before">

```json title="package.json"
{
  "devDependencies": {
    "typescript": "^5.1.0",
  }
}
```

</TabItem>
<TabItem value="after" label="After">

```json title="package.json"
{
  "devDependencies": {
    "typescript": "5.8.2",
  }
}
```

</TabItem>
</Tabs>

### 5. Tell Wasp about `jest-dom` types

If you're using (or planning to use) Wasp's [client tests](../project/testing.md) with `jest-dom`,
update your `src/vite-env.d.ts` file:

```ts src/vite-env.d.ts {3-7}
/// <reference types="vite/client" />

// This is needed to properly support Vitest testing with jest-dom matchers.
// Types for jest-dom are not recognized automatically and Typescript complains
// about missing types e.g. when using `toBeInTheDocument` and other matchers.
// Reference: https://github.com/testing-library/jest-dom/issues/546#issuecomment-1889884843
import "@testing-library/jest-dom";
```

### 6. Add a `favicon.ico` to the `public` directory

This step is necessary only if you don't have a `favicon.ico` in your `public` folder.
If so, you should add a `favicon.ico` to your `public` folder.

If you want to keep the default, you can [download it here](https://raw.githubusercontent.com/wasp-lang/wasp/refs/heads/main/waspc/data/Cli/starters/skeleton/public/favicon.ico).

If you want to generate a `favicon.ico` and all its possible variants, check out [RealFaviconGenerator](https://realfavicongenerator.net/), a handy open-source tool for creating favicons.

### 7. Add a `<link>` meta tag for `favicon.ico`

This step is required for all of the project's which use `favicon.ico`.
Add the `<link>` meta tag to the `head` property in the `main.wasp`

```wasp title="main.wasp
app MyApp {
  // ...
  head: [
    // highlight-next-line
    "<link rel='icon' href='/favicon.ico' />",
  ]
}
```

### 8. Upgrade Express dependencies

If you had `express` or `@types/express` in your `package.json`, you should change them to use version 5:

<Tabs>
<TabItem value="before" label="Before">

```json title="package.json"
{
  "dependencies": {
    "express": "~4.21.0"
  },
  "devDependencies": {
    "@types/express": "^4.17.13"
  }
}
```

</TabItem>
<TabItem value="after" label="After">

```json title="package.json"
{
  "dependencies": {
    "express": "~5.1.0"
  },
  "devDependencies": {
    "@types/express": "^5.0.0"
  }
}
```

</TabItem>
</Tabs>

### 9. Upgrade your `api` endpoints to Express 5

Wasp now uses [Express v5](https://expressjs.com/2024/10/15/v5-release.html), which impacts
[API Endpoints](../advanced/apis.md) (defined with `api` in your Wasp file).
[Operations](../data-model/operations/overview.md) (defined with `query` and `action` in your Wasp file)
are not affected by this change.

To upgrade, follow [Express's v5 migration guide](https://expressjs.com/en/guide/migrating-5.html).

:::tip
In general, you only need to worry about changes to the `req` and `res` objects in your API endpoints.
The breaking changes are mostly edge cases and most code should work without any updates.
:::

### 10. Enjoy your updated Wasp app

That's it!

You should now be able to run your app with the new Wasp 0.17.0.
