---
title: Auth Hooks
---

import { EmailPill, UsernameAndPasswordPill, GithubPill, GooglePill, KeycloakPill } from "./Pills";
import ImgWithCaption from '@site/blog/components/ImgWithCaption'

Auth hooks allow you to "hook into" the auth process at various stages and run your custom code. For example, if you want to forbid certain emails from signing up, or if you want to send a welcome email to the user after they sign up, auth hooks are the way to go.

## Using hooks

To use auth hooks, you must first declare them in the Wasp file:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

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
    onBeforeSignup: import { onBeforeSignup } from "@src/auth/hooks",
    onAfterSignup: import { onAfterSignup } from "@src/auth/hooks",
    onBeforeOAuthRedirect: import { onBeforeOAuthRedirect } from "@src/auth/hooks",
  },
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

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
    onBeforeSignup: import { onBeforeSignup } from "@src/auth/hooks",
    onAfterSignup: import { onAfterSignup } from "@src/auth/hooks",
    onBeforeOAuthRedirect: import { onBeforeOAuthRedirect } from "@src/auth/hooks",
  },
}
```
</TabItem>
</Tabs>

If the hooks are defined as async functions, Wasp _awaits_ them. This means the auth process waits for the hooks to finish before continuing.

Wasp ignores the return values of the hooks. The only exception is the `onBeforeOAuthRedirect` hook, whose return value affects the OAuth redirect URL.

### When are the hooks called

Below you can see the flow of the auth process with hooks:

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

## Available hooks

Wasp supports the following auth hooks:
- `onBeforeSignup`
- `onAfterSignup`
- `onBeforeOAuthRedirect`

Let's go through each of them.

### Executing code before the user signs up

Wasp calls the `onBeforeSignup` hook before the user is created.

The `onBeforeSignup` hook can be useful if you want to reject a user based on some criteria before they sign up.

Works with <EmailPill /> <UsernameAndPasswordPill /> <GithubPill /> <GooglePill /> <KeycloakPill />

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

export const onBeforeSignup = async ({
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

  if (providerId.providerName === 'email' && providerId.providerUserId === 'some@email.com') {
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
import type { OnBeforeSignupHook } from 'wasp/server/auth'

export const onBeforeSignup: OnBeforeSignupHook = async ({
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

  if (providerId.providerName === 'email' && providerId.providerUserId === 'some@email.com') {
    throw new HttpError(403, 'This email is not allowed')
  }
}
```

</TabItem>
</Tabs>

Read more about the data the `onBeforeSignup` hook receives in the [API Reference](#the-onbeforesignup-hook).

### Executing code after the user signs up

Wasp calls the `onAfterSignup` hook after the user is created.

The `onAfterSignup` hook can be useful if you want to send the user a welcome email or perform some other action after the user signs up like syncing the user with a third-party service.

Since the `onAfterSignup` hook receives the OAuth access token, it can also be used to store the OAuth access token for the user in your database.

Works with <EmailPill /> <UsernameAndPasswordPill /> <GithubPill /> <GooglePill /> <KeycloakPill />

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
import type { OnAfterSignupHook } from 'wasp/server/auth'

export const onAfterSignup: OnAfterSignupHook = async ({
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

Read more about the data the `onAfterSignup` hook receives in the [API Reference](#the-onaftersignup-hook).

### Executing code before the OAuth redirect

Wasp calls the `onBeforeOAuthRedirect` hook after the OAuth redirect URL is generated but before the user is redirected to it. This hook has access to the request object that was sent from the client when the OAuth flow was initiated.

The `onBeforeOAuthRedirect` hook can be useful if you want to save some data (e.g. request query parameters) that can be used later in the OAuth flow. You can use the `uniqueRequestId` parameter to reference this data later in the `onAfterSignup` hook.

Works with <GithubPill /> <GooglePill /> <KeycloakPill />

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
import type { OnBeforeOAuthRedirectHook } from 'wasp/server/auth'

export const onBeforeOAuthRedirect: OnBeforeOAuthRedirectHook = async ({
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

The **return value** of this hook must be an object that looks like this: `{ url: URL }`. This URL object is used to redirect the user to the OAuth provider.

Read more about the data the `onBeforeOAuthRedirect` hook receives in the [API Reference](#the-onbeforeoauthredirect-hook).

## API Reference

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

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
    onBeforeSignup: import { onBeforeSignup } from "@src/auth/hooks",
    onAfterSignup: import { onAfterSignup } from "@src/auth/hooks",
    onBeforeOAuthRedirect: import { onBeforeOAuthRedirect } from "@src/auth/hooks",
  },
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

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
    onBeforeSignup: import { onBeforeSignup } from "@src/auth/hooks",
    onAfterSignup: import { onAfterSignup } from "@src/auth/hooks",
    onBeforeOAuthRedirect: import { onBeforeOAuthRedirect } from "@src/auth/hooks",
  },
}
```
</TabItem>
</Tabs>

### The `onBeforeSignup` hook

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/auth/hooks.js"
import { HttpError } from 'wasp/server'

export const onBeforeSignup = async ({
  providerId,
  prisma,
  req,
  hookName,
}) => {
  // Hook code here
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/auth/hooks.ts"
import { HttpError } from 'wasp/server'
import type { OnBeforeSignupHook } from 'wasp/server/auth'

export const onBeforeSignup: OnBeforeSignupHook = async ({
  providerId,
  prisma,
  req,
  hookName,
}) => {
  // Hook code here
}
```

</TabItem>
</Tabs>

The hook receives an object as **input** with the following properties:

- `providerId: ProviderId`

  The user's provider ID is an object with two properties:
  - `providerName: string`

    The name of the provider (e.g. `'email'`, `'google'`, `'github'`)
  - `providerUserId: string`
    
    The unique ID of the user in the provider's system (e.g. email, Google ID, GitHub ID)
- `prisma: PrismaClient`

  Prisma client instance which you can use to query your database.
- `req: Request`

  [Express request object](https://expressjs.com/en/api.html#req) from which you can access the request headers, cookies, etc.
- `hookName: string`

  The name of the current hook (`'onBeforeSignup'`). This value can be useful when logging or debugging.

Wasp ignores this hook's **return value**.

### The `onAfterSignup` hook

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/auth/hooks.js"
export const onAfterSignup = async ({
  providerId,
  user,
  oauth,
  prisma,
  req,
  hookName,
}) => {
  // Hook code here
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/auth/hooks.ts"
import type { OnAfterSignupHook } from 'wasp/server/auth'

export const onAfterSignup: OnAfterSignupHook = async ({
  providerId,
  user,
  oauth,
  prisma,
  req,
  hookName,
}) => {
  // Hook code here
}
```

</TabItem>
</Tabs>

The hook receives an object as **input** with the following properties:
- `providerId: ProviderId`

  The user's provider ID is an object with two properties:
  - `providerName: string`
  
    The name of the provider (e.g. `'email'`, `'google'`, `'github'`)
  - `providerUserId: string`
  
  The unique ID of the user in the provider's system (e.g. email, Google ID, GitHub ID)
- `user: User`
  
  The user object that was created.
- `oauth?: OAuthFields`

  This object is present only when the user is created using [Social Auth](./social-auth/overview.md).
  It contains the following fields:
  - `accessToken: string`

    The OAuth access token can be used to make requests to the provider's API on behalf of the user.
  - `uniqueRequestId: string`
  
      The unique request ID for the OAuth flow. (You might know it as the `state` parameter in OAuth.)
      
      The `onBeforeOAuthRedirect` hook receives the same value which can be used to reference data saved in that hook.
- `prisma: PrismaClient`

  Prisma client instance which you can use to query your database.
- `req: Request`
  
  [Express request object](https://expressjs.com/en/api.html#req) from which you can access the request headers, cookies, etc.
- `hookName: string`

  The name of the current hook (`'onAfterSignup'`). This value can be useful when logging or debugging.

Wasp ignores this hook's **return value**.

### The `onBeforeOAuthRedirect` hook

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/auth/hooks.js"
export const onBeforeOAuthRedirect = async ({
  url,
  uniqueRequestId,
  prisma,
  req,
  hookName,
}) => {
  // Hook code here

  return { url }
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/auth/hooks.ts"
import type { OnBeforeOAuthRedirectHook } from 'wasp/server/auth'

export const onBeforeOAuthRedirect: OnBeforeOAuthRedirectHook = async ({
  url,
  uniqueRequestId,
  prisma,
  req,
  hookName,
}) => {
  // Hook code here

  return { url }
}
```

</TabItem>
</Tabs>

The hook receives an object as **input** with the following properties:
- `url: URL`

    The URL object that is used for the OAuth redirect.
- `uniqueRequestId: string`

    The unique request ID for the OAuth flow. (You might know it as the `state` parameter in OAuth.)

    The`onAfterSignup` hook receives the same value which can be used to reference data saved in this hook.
- `prisma: PrismaClient`
    
    Prisma client instance which you can use to query your database.
- `req: Request`

  [Express request object](https://expressjs.com/en/api.html#req) from which you can access the request headers, cookies, etc.
- `hookName: string`
    
  The name of the current hook (`'onBeforeOAuthRedirect'`). This value can be useful when logging or debugging.

The **return value** of this hook must be an object that looks like this: `{ url: URL }`. This URL object is used to redirect the user to the OAuth provider.
