---
title: Auth Hooks
---

import { EmailPill, UsernameAndPasswordPill, GithubPill, GooglePill, KeycloakPill } from "./Pills";


Sometimes you need to do some action _before_ the user signs up or _after_ they sign up. Or you need to access the OAuth redirect URL or the OAuth access token. For these cases, you can use the Auth Hooks.

## Available Hooks

For every auth hook you want to use, you need to declare it in the `auth` dict in your Wasp file:

```wasp
app myApp {
  wasp: {
    version: "^0.13.0"
  },
  auth: {
    userEntity: User,
    methods: {
      ...
    },
    onBeforeSignup: import { onBeforeSignup } from "@src/auth/hooks.js",
    onAfterSignup: import { onAfterSignup } from "@src/auth/hooks.js",
    onBeforeOAuthRedirect: import { onBeforeOAuthRedirect } from "@src/auth/hooks.js",
    onAfterOAuthTokenReceived: import { onAfterOAuthTokenReceived } from "@src/auth/hooks.js",
  },
}
```

Wasp _awaits_ the async hook functions in various stages of the auth process. The hooks are called with an `args` object that contains the necessary information for the hook to do its job. 

The return value is ignored for all hooks except `onBeforeOAuthRedirect` which enables you to modify the OAuth redirect URL.


### `onBeforeSignup` hook


This hook is called before the user signs up. It is an async function that Wasp awaits before proceeding with the signup.


:::info

It can be useful if you want to reject a user based on some criteria before they sign up.
:::

Works with <EmailPill /> <UsernameAndPasswordPill /> <GithubPill /> <GooglePill /> <KeycloakPill />

Its **input** is an `args` object with the following properties:

- `providerId: { providerName: string, providerUserId: string }` - The provider ID of the user
- `prisma: PrismaClient` - Prisma client instance
- `req: Request` - Express request object

The **return value** of this hook is ignored.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app myApp {
  ...
  auth: {
    ...
    onBeforeSignup: import { onBeforeSignup } from "@src/auth/hooks",
  },
}
```

```js title="src/auth/hooks.js"
export const onBeforeSignup = async ({ providerId, prisma, req }) => {
  // Do something before signup
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
app myApp {
  ...
  auth: {
    ...
    onBeforeSignup: import { onBeforeSignup } from "@src/auth/hooks",
  },
}
```

```ts title="src/auth/hooks.ts"
import type { OnBeforeSignupHookFn } from 'wasp/server/auth'

export const onBeforeSignup: OnBeforeSignupHookFn = async ({ providerId, prisma, req }) => {
  // Do something before signup
}
```

</TabItem>
</Tabs>


### `onAfterSignup` hook


This hook is called after the user signs up. It is an async function that Wasp awaits before proceeding with the signup.

:::info

It can be useful if you want to send the user a welcome email or perform some other action after the user signs up like syncing the user with a third-party service.
:::

Works with <EmailPill /> <UsernameAndPasswordPill /> <GithubPill /> <GooglePill /> <KeycloakPill />

Its **input** is an `args` object with the following properties:
- `providerId: { providerName: string, providerUserId: string }` - The provider ID of the user
- `user: User` - The user object that was created
- `prisma: PrismaClient` - Prisma client instance
- `req: Request` - Express request object

The **return value** of this hook is ignored.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app myApp {
  ...
  auth: {
    ...
    onAfterSignup: import { onAfterSignup } from "@src/auth/hooks",
  },
}
```

```js title="src/auth/hooks.js"
export const onAfterSignup = async ({ providerId, user, prisma, req }) => {
  // Do something after signup
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
app myApp {
  ...
  auth: {
    ...
    onAfterSignup: import { onAfterSignup } from "@src/auth/hooks",
  },
}
```

```ts title="src/auth/hooks.ts"
import type { OnAfterSignupHookFn } from 'wasp/server/auth'

export const onAfterSignup: OnAfterSignupHookFn = async ({ providerId, user, prisma, req }) => {
  // Do something after signup
}
```

</TabItem>
</Tabs>

### `onBeforeOAuthRedirect` hook


This hook is called before the OAuth redirect URL is generated. It is an async function that Wasp awaits before proceeding with the OAuth redirect.

:::info

If can be useful if you want to add some query params to the OAuth redirect URL which can be used later in the OAuth callback.
:::

Works with <GithubPill /> <GooglePill /> <KeycloakPill />

Its **input** is an `args` object with the following properties:
- `url: URL` - The URL object that will be used for the OAuth redirect
- `prisma: PrismaClient` - Prisma client instance
- `req: Request` - Express request object

The **return value** of this hook should be an object with a `url` property that is a URL object. This URL object will be used for the OAuth redirect.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app myApp {
  ...
  auth: {
    ...
    onBeforeOAuthRedirect: import { onBeforeOAuthRedirect } from "@src/auth/hooks",
  },
}
```

```js title="src/auth/hooks.js"
export const onBeforeOAuthRedirect = async ({ url, prisma, req }) => {
  // Do something before OAuth redirect
  return { url }
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
app myApp {
  ...
  auth: {
    ...
    onBeforeOAuthRedirect: import { onBeforeOAuthRedirect } from "@src/auth/hooks",
  },
}
```

```ts title="src/auth/hooks.ts"
import type { OnBeforeOAuthRedirectHookFn } from 'wasp/server/auth'

export const onBeforeOAuthRedirect: OnBeforeOAuthRedirectHookFn = async ({ url, prisma, req }) => {
  // Do something before OAuth redirect
  return { url }
}
```

</TabItem>
</Tabs>

### `onAfterOAuthTokenReceived` hook


This hook is called after the OAuth access token is received. It is an async function that Wasp awaits before proceeding with the OAuth token.

:::info

It can be useful if you want to do something after the OAuth token is received like saving the token to the database.
:::

Works with <GithubPill /> <GooglePill /> <KeycloakPill />

Its **input** is an `args` object with the following properties:
- `accessToken: string` - The OAuth
- `prisma: PrismaClient` - Prisma client instance
- `req: Request` - Express request object

The **return value** of this hook is ignored.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app myApp {
  ...
  auth: {
    ...
    onAfterOAuthTokenReceived: import { onAfterOAuthTokenReceived } from "@src/auth/hooks",
  },
}
```

```js title="src/auth/hooks.js"
export const onAfterOAuthTokenReceived = async ({ accessToken, prisma, req }) => {
  // Do something after OAuth token received
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
app myApp {
  ...
  auth: {
    ...
    onAfterOAuthTokenReceived: import { onAfterOAuthTokenReceived } from "@src/auth/hooks",
  },
}
```

```ts title="src/auth/hooks.ts"
import type { OnAfterOAuthTokenReceivedHookFn } from 'wasp/server/auth'

export const onAfterOAuthTokenReceived: OnAfterOAuthTokenReceivedHookFn = async ({ accessToken, prisma, req }) => {
  // Do something after OAuth token received
}
```

</TabItem>
</Tabs>
