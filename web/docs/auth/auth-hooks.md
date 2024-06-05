---
title: Auth Hooks
---

import { EmailPill, UsernameAndPasswordPill, GithubPill, GooglePill, KeycloakPill } from "./Pills";
import ImgWithCaption from '@site/blog/components/ImgWithCaption'

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
  },
}
```

Wasp _awaits_ the async hook functions in various stages of the auth process. The hooks are called with an `args` object that contains the necessary information for the hook to do its job.

The return value is ignored for all hooks except `onBeforeOAuthRedirect` which enables you to modify the OAuth redirect URL.

#### When Are the Hooks Called

<ImgWithCaption
  source="/img/auth-hooks/signup_flow_with_hooks.png"
  alt="Signup Flow with Hooks"
  caption="Signup Flow with Hooks"
/>

<ImgWithCaption
  source="/img/auth-hooks/oauth_flow_with_hooks.png"
  alt="OAuth Flow with Hooks"
  caption="OAuth Flow with Hooks"
/>

### `onBeforeSignup` hook

This hook is called before the user signs up. It is an async function that Wasp awaits before proceeding with the signup.

:::info

It can be useful if you want to reject a user based on some criteria before they sign up.
:::

Works with <EmailPill /> <UsernameAndPasswordPill /> <GithubPill /> <GooglePill /> <KeycloakPill />

Its **input** is an `args` object with the following properties:

- `providerId: { providerName: string, providerUserId: string }`

  The provider ID of the user. It is an object with two properties:

  - `providerName: string` - The name of the provider (e.g. `'email'`, `'google'`, `'github'`)
  - `providerUserId: string` - The unique ID of the user in the provider's system (e.g. email, Google ID, GitHub ID)

- `prisma: PrismaClient`

  Prisma client instance. You can use it to query your database.

- `req: Request`

  Express request object. You can access the request headers, cookies, etc. from this object.

- `hookName: string`

  The name of the hook that is being called (in this case, it will be `'onBeforeSignup'`).

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
import { HttpError } from 'wasp/server'

export const onBeforeSignup = async ({ providerId, prisma, req, hookName }) => {
  const count = await prisma.user.count()
  console.log('number of users before', count)
  console.log('provider name', providerId.providerName)
  console.log('provider user ID', providerId.providerUserId)

  if (count > 100) {
    throw new HttpError(403, 'Too many users')
  }

  if (
    providerId.providerName === 'email' &&
    providerId.providerUserId === 'some@email.com'
  ) {
    throw new HttpError(403, 'This email is not allowed')
  }
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
import { HttpError } from 'wasp/server'
import type { OnBeforeSignupHookFn } from 'wasp/server/auth'

export const onBeforeSignup: OnBeforeSignupHookFn = async ({
  providerId,
  prisma,
  req,
  hookName,
}) => {
  const count = await prisma.user.count()
  console.log('number of users before', count)
  console.log('provider name', providerId.providerName)
  console.log('provider user ID', providerId.providerUserId)

  if (count > 100) {
    throw new HttpError(403, 'Too many users')
  }

  if (
    providerId.providerName === 'email' &&
    providerId.providerUserId === 'some@email.com'
  ) {
    throw new HttpError(403, 'This email is not allowed')
  }
}
```

</TabItem>
</Tabs>

### `onAfterSignup` hook

This hook is called after the user signs up. It is an async function that Wasp awaits before proceeding with the signup.

:::info

It can be useful if you want to send the user a welcome email or perform some other action after the user signs up like syncing the user with a third-party service.

It can also be useful to store the OAuth access token for the user in your database.
:::

Works with <EmailPill /> <UsernameAndPasswordPill /> <GithubPill /> <GooglePill /> <KeycloakPill />

Its **input** is an `args` object with the following properties:

- `providerId: { providerName: string, providerUserId: string }`

  The provider ID of the user. It is an object with two properties:

  - `providerName: string` - The name of the provider (e.g. `'email'`, `'google'`, `'github'`)
  - `providerUserId: string` - The unique ID of the user in the provider's system (e.g. email, Google ID, GitHub ID)

- `user: User`

  The user object that was created.

- `oauth?: OAuthFields`

  This object is present only when the user is created using [Social Auth](./social-auth/overview.md).
  It contains the following fields:

  - `accessToken: string`

    The OAuth access token. It can be used to make requests to the provider's API on behalf of the user.

  - `uniqueRequestId: string`

    The unique request ID for the OAuth flow. (Some might know it as the `state` parameter in OAuth.)

    It's the same value that was passed to `onBeforeOAuthRedirect` hook.

- `prisma: PrismaClient`

  Prisma client instance. You can use it to query your database.

- `req: Request`

  Express request object. You can access the request headers, cookies, etc. from this object.

- `hookName: string`

  The name of the hook that is being called (in this case, it will be `'onAfterSignup'`).

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
export const onAfterSignup = async ({
  providerId,
  user,
  oauth,
  prisma,
  req,
  hookName,
}) => {
  const count = await prisma.user.count()
  console.log('number of users after', count)
  console.log('user object', user)

  // If this is a OAuth signup, we have the access token and uniqueRequestId
  if (oauth) {
    console.log('accessToken', oauth.accessToken)
    console.log('uniqueRequestId', oauth.uniqueRequestId)

    const id = oauth.uniqueRequestId
    const data = someKindOfStore.get(id)
    if (data) {
      console.log('saved data for the ID', data)
    }
    someKindOfStore.delete(id)
  }
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

export const onAfterSignup: OnAfterSignupHookFn = async ({
  providerId,
  user,
  oauth,
  prisma,
  req,
  hookName,
}) => {
  const count = await prisma.user.count()
  console.log('number of users after', count)
  console.log('user object', user)

  // If this is a OAuth signup, we have the access token and uniqueRequestId
  if (oauth) {
    console.log('accessToken', oauth.accessToken)
    console.log('uniqueRequestId', oauth.uniqueRequestId)

    const id = oauth.uniqueRequestId
    const data = someKindOfStore.get(id)
    if (data) {
      console.log('saved data for the ID', data)
    }
    someKindOfStore.delete(id)
  }
}
```

</TabItem>
</Tabs>

### `onBeforeOAuthRedirect` hook

This hook is called before the OAuth redirect URL is generated. It is an async function that Wasp awaits before proceeding with the OAuth redirect.

:::info

It can be useful if you want to save some data in the database that can be used later in the OAuth flow. For that, you can use the `uniqueRequestId` parameter.
:::

Works with <GithubPill /> <GooglePill /> <KeycloakPill />

Its **input** is an `args` object with the following properties:

- `url: URL`

  The URL object that will be used for the OAuth redirect.

- `uniqueRequestId: string`

  The unique request ID for the OAuth flow. (Some might know it as `state` parameter in OAuth.)

  This is the same value that will be passed to `onAfterSignup` hook and can be used to save some user data temporarily that can be used later.

- `prisma: PrismaClient`
  Prisma client instance. You can use it to query your database.
- `req: Request`

  Express request object. You can access the request headers, cookies, etc. from this object.

- `hookName: string`
  The name of the hook that is being called (in this case, it will be `'onBeforeOAuthRedirect'`)

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
export const onBeforeOAuthRedirect = async ({
  url,
  uniqueRequestId,
  prisma,
  req,
  hookName,
}) => {
  console.log('query params before oAuth redirect', req.query)

  // Saving query params for later use in onAfterSignup hook
  const id = uniqueRequestId
  someKindOfStore.set(id, req.query)

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

export const onBeforeOAuthRedirect: OnBeforeOAuthRedirectHookFn = async ({
  url,
  uniqueRequestId,
  prisma,
  req,
  hookName,
}) => {
  console.log('query params before oAuth redirect', req.query)

  // Saving query params for later use in onAfterSignup hook
  const id = uniqueRequestId
  someKindOfStore.set(id, req.query)

  return { url }
}
```

</TabItem>
</Tabs>
