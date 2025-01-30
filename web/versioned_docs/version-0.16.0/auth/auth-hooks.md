---
title: Auth Hooks
---

import { EmailPill, UsernameAndPasswordPill, GithubPill, GooglePill, KeycloakPill, DiscordPill } from "./Pills";
import ImgWithCaption from '@site/blog/components/ImgWithCaption'
import { ShowForTs } from '@site/src/components/TsJsHelpers'

Auth hooks allow you to "hook into" the auth process at various stages and run your custom code. For example, if you want to forbid certain emails from signing up, or if you wish to send a welcome email to the user after they sign up, auth hooks are the way to go.

## Supported hooks

The following auth hooks are available in Wasp:

- [`onBeforeSignup`](#executing-code-before-the-user-signs-up)
- [`onAfterSignup`](#executing-code-after-the-user-signs-up)
- [`onBeforeOAuthRedirect`](#executing-code-before-the-oauth-redirect)
- [`onBeforeLogin`](#executing-code-before-the-user-logs-in)
- [`onAfterLogin`](#executing-code-after-the-user-logs-in)

We'll go through each of these hooks in detail. But first, let's see how the hooks fit into the auth flows:

<ImgWithCaption
  source="/img/auth-hooks/signup_flow_with_hooks.png"
  alt="Signup Flow with Hooks"
  caption="Signup Flow with Hooks"
/>

<ImgWithCaption
  source="/img/auth-hooks/login_flow_with_hooks.png"
  alt="Login Flow with Hooks"
  caption="Login Flow with Hooks *"
/>

<small>

\* When using the OAuth auth providers, the login hooks are both called before the session is created but the session is created quickly afterward, so it shouldn't make any difference in practice.
</small>

If you are using OAuth, the flow includes extra steps before the auth flow:

<ImgWithCaption
  source="/img/auth-hooks/oauth_flow_with_hooks.png"
  alt="OAuth Flow with Hooks"
  caption="OAuth Flow with Hooks"
/>

## Using hooks

To use auth hooks, you must first declare them in the Wasp file:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp
app myApp {
  wasp: {
    version: "^0.15.0"
  },
  auth: {
    userEntity: User,
    methods: {
      ...
    },
    onBeforeSignup: import { onBeforeSignup } from "@src/auth/hooks",
    onAfterSignup: import { onAfterSignup } from "@src/auth/hooks",
    onBeforeOAuthRedirect: import { onBeforeOAuthRedirect } from "@src/auth/hooks",
    onBeforeLogin: import { onBeforeLogin } from "@src/auth/hooks",
    onAfterLogin: import { onAfterLogin } from "@src/auth/hooks",
  },
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp
app myApp {
  wasp: {
    version: "^0.15.0"
  },
  auth: {
    userEntity: User,
    methods: {
      ...
    },
    onBeforeSignup: import { onBeforeSignup } from "@src/auth/hooks",
    onAfterSignup: import { onAfterSignup } from "@src/auth/hooks",
    onBeforeOAuthRedirect: import { onBeforeOAuthRedirect } from "@src/auth/hooks",
    onBeforeLogin: import { onBeforeLogin } from "@src/auth/hooks",
    onAfterLogin: import { onAfterLogin } from "@src/auth/hooks",
  },
}
```

</TabItem>
</Tabs>

If the hooks are defined as async functions, Wasp _awaits_ them. This means the auth process waits for the hooks to finish before continuing.

Wasp ignores the hooks' return values. The only exception is the `onBeforeOAuthRedirect` hook, whose return value affects the OAuth redirect URL.

We'll now go through each of the available hooks.

### Executing code before the user signs up

Wasp calls the `onBeforeSignup` hook before the user is created.

The `onBeforeSignup` hook can be useful if you want to reject a user based on some criteria before they sign up.

Works with <EmailPill /> <UsernameAndPasswordPill /> <DiscordPill /> <GithubPill /> <GooglePill /> <KeycloakPill />

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

export const onBeforeSignup = async ({ providerId, prisma, req }) => {
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
import type { OnBeforeSignupHook } from 'wasp/server/auth'

export const onBeforeSignup: OnBeforeSignupHook = async ({
  providerId,
  prisma,
  req,
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

Read more about the data the `onBeforeSignup` hook receives in the [API Reference](#the-onbeforesignup-hook).

### Executing code after the user signs up

Wasp calls the `onAfterSignup` hook after the user is created.

The `onAfterSignup` hook can be useful if you want to send the user a welcome email or perform some other action after the user signs up like syncing the user with a third-party service.

Since the `onAfterSignup` hook receives the OAuth tokens, you can use this hook to store the OAuth access token and/or [refresh token](#refreshing-the-oauth-access-token) in your database.

Works with <EmailPill /> <UsernameAndPasswordPill /> <DiscordPill /> <GithubPill /> <GooglePill /> <KeycloakPill />

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
}) => {
  const count = await prisma.user.count()
  console.log('number of users after', count)
  console.log('user object', user)

  // If this is an OAuth signup, you have access to the OAuth tokens and the uniqueRequestId
  if (oauth) {
    console.log('accessToken', oauth.tokens.accessToken)
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
}) => {
  const count = await prisma.user.count()
  console.log('number of users after', count)
  console.log('user object', user)

  // If this is an OAuth signup, you have access to the OAuth tokens and the uniqueRequestId
  if (oauth) {
    console.log('accessToken', oauth.tokens.accessToken)
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

Wasp calls the `onBeforeOAuthRedirect` hook after the OAuth redirect URL is generated but before redirecting the user. This hook can access the request object sent from the client at the start of the OAuth process.

The `onBeforeOAuthRedirect` hook can be useful if you want to save some data (e.g. request query parameters) that you can use later in the OAuth flow. You can use the `uniqueRequestId` parameter to reference this data later in the `onAfterSignup` or `onAfterLogin` hooks.

Works with <DiscordPill /> <GithubPill /> <GooglePill /> <KeycloakPill />

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
export const onBeforeOAuthRedirect = async ({ url, oauth, prisma, req }) => {
  console.log('query params before oAuth redirect', req.query)

  // Saving query params for later use in onAfterSignup or onAfterLogin hooks
  const id = oauth.uniqueRequestId
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
  oauth,
  prisma,
  req,
}) => {
  console.log('query params before oAuth redirect', req.query)

  // Saving query params for later use in onAfterSignup or onAfterLogin hooks
  const id = oauth.uniqueRequestId
  someKindOfStore.set(id, req.query)

  return { url }
}
```

</TabItem>
</Tabs>

This hook's return value must be an object that looks like this: `{ url: URL }`. Wasp uses the URL to redirect the user to the OAuth provider.

Read more about the data the `onBeforeOAuthRedirect` hook receives in the [API Reference](#the-onbeforeoauthredirect-hook).

### Executing code before the user logs in

Wasp calls the `onBeforeLogin` hook before the user is logged in.

The `onBeforeLogin` hook can be useful if you want to reject a user based on some criteria before they log in.

Works with <EmailPill /> <UsernameAndPasswordPill /> <DiscordPill /> <GithubPill /> <GooglePill /> <KeycloakPill />

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app myApp {
  ...
  auth: {
    ...
    onBeforeLogin: import { onBeforeLogin } from "@src/auth/hooks",
  },
}
```

```js title="src/auth/hooks.js"
import { HttpError } from 'wasp/server'

export const onBeforeLogin = async ({ providerId, user, prisma, req }) => {
  if (
    providerId.providerName === 'email' &&
    providerId.providerUserId === 'some@email.com'
  ) {
    throw new HttpError(403, 'You cannot log in with this email')
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
    onBeforeLogin: import { onBeforeLogin } from "@src/auth/hooks",
  },
}
```

```ts title="src/auth/hooks.ts"
import { HttpError } from 'wasp/server'
import type { OnBeforeLoginHook } from 'wasp/server/auth'

export const onBeforeLogin: OnBeforeLoginHook = async ({
  providerId,
  user,
  prisma,
  req,
}) => {
  if (
    providerId.providerName === 'email' &&
    providerId.providerUserId === 'some@email.com'
  ) {
    throw new HttpError(403, 'You cannot log in with this email')
  }
}
```

</TabItem>
</Tabs>

Read more about the data the `onBeforeLogin` hook receives in the [API Reference](#the-onbeforelogin-hook).

### Executing code after the user logs in

Wasp calls the `onAfterLogin` hook after the user logs in.

The `onAfterLogin` hook can be useful if you want to perform some action after the user logs in, like syncing the user with a third-party service.

Since the `onAfterLogin` hook receives the OAuth tokens, you can use it to update the OAuth access token for the user in your database. You can also use it to [refresh the OAuth access token](#refreshing-the-oauth-access-token) if the provider supports it.

Works with <EmailPill /> <UsernameAndPasswordPill /> <DiscordPill /> <GithubPill /> <GooglePill /> <KeycloakPill />

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app myApp {
  ...
  auth: {
    ...
    onAfterLogin: import { onAfterLogin } from "@src/auth/hooks",
  },
}
```

```js title="src/auth/hooks.js"
export const onAfterLogin = async ({
  providerId,
  user,
  oauth,
  prisma,
  req,
}) => {
  console.log('user object', user)

  // If this is an OAuth signup, you have access to the OAuth tokens and the uniqueRequestId
  if (oauth) {
    console.log('accessToken', oauth.tokens.accessToken)
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
    onAfterLogin: import { onAfterLogin } from "@src/auth/hooks",
  },
}
```

```ts title="src/auth/hooks.ts"
import type { OnAfterLoginHook } from 'wasp/server/auth'

export const onAfterLogin: OnAfterLoginHook = async ({
  providerId,
  user,
  oauth,
  prisma,
  req,
}) => {
  console.log('user object', user)

  // If this is an OAuth signup, you have access to the OAuth tokens and the uniqueRequestId
  if (oauth) {
    console.log('accessToken', oauth.tokens.accessToken)
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

Read more about the data the `onAfterLogin` hook receives in the [API Reference](#the-onafterlogin-hook).

### Refreshing the OAuth access token

Some OAuth providers support refreshing the access token when it expires. To refresh the access token, you need the OAuth **refresh token**.

Wasp exposes the OAuth refresh token in the `onAfterSignup` and `onAfterLogin` hooks. You can store the refresh token in your database and use it to refresh the access token when it expires.

Import the provider object with the OAuth client from the `wasp/server/auth` module. For example, to refresh the Google OAuth access token, import the `google` object from the `wasp/server/auth` module. You use the `refreshAccessToken` method of the OAuth client to refresh the access token.

Here's an example of how you can refresh the access token for Google OAuth:

<Tabs groupId="js-ts">

<TabItem value="js" label="JavaScript">

```js title="src/auth/hooks.js"
import { google } from 'wasp/server/auth'

export const onAfterLogin = async ({ oauth }) => {
  if (oauth.provider === 'google' && oauth.tokens.refreshToken !== null) {
    const newTokens = await google.oAuthClient.refreshAccessToken(
      oauth.tokens.refreshToken
    )
    log('new tokens', newTokens)
  }
}
```

</TabItem>

<TabItem value="ts" label="TypeScript">

```ts title="src/auth/hooks.ts"
import type { OnAfterLoginHook } from 'wasp/server/auth'
import { google } from 'wasp/server/auth'

export const onAfterLogin: OnAfterLoginHook = async ({ oauth }) => {
  if (oauth.provider === 'google' && oauth.tokens.refreshToken !== null) {
    const newTokens = await google.oAuthClient.refreshAccessToken(
      oauth.tokens.refreshToken
    )
    log('new tokens', newTokens)
  }
}
```

</TabItem>
</Tabs>

Google exposes the `accessTokenExpiresAt` field in the `oauth.tokens` object. You can use this field to determine when the access token expires.

If you want to refresh the token periodically, use a [Wasp Job](../advanced/jobs.md).

## API Reference

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp
app myApp {
  wasp: {
    version: "^0.15.0"
  },
  auth: {
    userEntity: User,
    methods: {
      ...
    },
    onBeforeSignup: import { onBeforeSignup } from "@src/auth/hooks",
    onAfterSignup: import { onAfterSignup } from "@src/auth/hooks",
    onBeforeOAuthRedirect: import { onBeforeOAuthRedirect } from "@src/auth/hooks",
    onBeforeLogin: import { onBeforeLogin } from "@src/auth/hooks",
    onAfterLogin: import { onAfterLogin } from "@src/auth/hooks",
  },
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp
app myApp {
  wasp: {
    version: "^0.15.0"
  },
  auth: {
    userEntity: User,
    methods: {
      ...
    },
    onBeforeSignup: import { onBeforeSignup } from "@src/auth/hooks",
    onAfterSignup: import { onAfterSignup } from "@src/auth/hooks",
    onBeforeOAuthRedirect: import { onBeforeOAuthRedirect } from "@src/auth/hooks",
    onBeforeLogin: import { onBeforeLogin } from "@src/auth/hooks",
    onAfterLogin: import { onAfterLogin } from "@src/auth/hooks",
  },
}
```

</TabItem>
</Tabs>

### Common hook input

The following properties are available in all auth hooks:

- `prisma: PrismaClient`

  The Prisma client instance which you can use to query your database.

- `req: Request`

  The [Express request object](https://expressjs.com/en/api.html#req) from which you can access the request headers, cookies, etc.

### The `onBeforeSignup` hook

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/auth/hooks.js"
export const onBeforeSignup = async ({ providerId, prisma, req }) => {
  // Hook code goes here
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/auth/hooks.ts"
import type { OnBeforeSignupHook } from 'wasp/server/auth'

export const onBeforeSignup: OnBeforeSignupHook = async ({
  providerId,
  prisma,
  req,
}) => {
  // Hook code goes here
}
```

</TabItem>
</Tabs>

The hook receives an object as **input** with the following properties:

- [`providerId: ProviderId`](#providerid-fields)

- Plus the [common hook input](#common-hook-input)

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
}) => {
  // Hook code goes here
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
}) => {
  // Hook code goes here
}
```

</TabItem>
</Tabs>

The hook receives an object as **input** with the following properties:

- [`providerId: ProviderId`](#providerid-fields)
- `user: User`

  The user object that was created.

- [`oauth?: OAuthFields`](#oauth-fields)

- Plus the [common hook input](#common-hook-input)

Wasp ignores this hook's **return value**.

### The `onBeforeOAuthRedirect` hook

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/auth/hooks.js"
export const onBeforeOAuthRedirect = async ({ url, oauth, prisma, req }) => {
  // Hook code goes here

  return { url }
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/auth/hooks.ts"
import type { OnBeforeOAuthRedirectHook } from 'wasp/server/auth'

export const onBeforeOAuthRedirect: OnBeforeOAuthRedirectHook = async ({
  url,
  oauth,
  prisma,
  req,
}) => {
  // Hook code goes here

  return { url }
}
```

</TabItem>
</Tabs>

The hook receives an object as **input** with the following properties:

- `url: URL`

  Wasp uses the URL for the OAuth redirect.

- `oauth: { uniqueRequestId: string }`

  The `oauth` object has the following fields:

  - `uniqueRequestId: string`

    The unique request ID for the OAuth flow (you might know it as the `state` parameter in OAuth.)

    You can use the unique request ID to save data (e.g. request query params) that you can later use in the `onAfterSignup` or `onAfterLogin` hooks.

- Plus the [common hook input](#common-hook-input)

This hook's return value must be an object that looks like this: `{ url: URL }`. Wasp uses the URL to redirect the user to the OAuth provider.

### The `onBeforeLogin` hook

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/auth/hooks.js"
export const onBeforeLogin = async ({ providerId, prisma, req }) => {
  // Hook code goes here
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/auth/hooks.ts"
import type { OnBeforeLoginHook } from 'wasp/server/auth'

export const onBeforeLogin: OnBeforeLoginHook = async ({
  providerId,
  prisma,
  req,
}) => {
  // Hook code goes here
}
```

</TabItem>
</Tabs>

The hook receives an object as **input** with the following properties:

- [`providerId: ProviderId`](#providerid-fields)

- `user: User`

  The user that is trying to log in.

- Plus the [common hook input](#common-hook-input)

Wasp ignores this hook's **return value**.

### The `onAfterLogin` hook

<Tabs groupId="js-ts">

<TabItem value="js" label="JavaScript">

```js title="src/auth/hooks.js"
export const onAfterLogin = async ({
  providerId,
  user,
  oauth,
  prisma,
  req,
}) => {
  // Hook code goes here
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/auth/hooks.ts"
import type { OnAfterLoginHook } from 'wasp/server/auth'

export const onAfterLogin: OnAfterLoginHook = async ({
  providerId,
  user,
  oauth,
  prisma,
  req,
}) => {
  // Hook code goes here
}
```

</TabItem>
</Tabs>

The hook receives an object as **input** with the following properties:

- [`providerId: ProviderId`](#providerid-fields)

- `user: User`

  The logged-in user's object.

- [`oauth?: OAuthFields`](#oauth-fields)

- Plus the [common hook input](#common-hook-input)

Wasp ignores this hook's **return value**.

### ProviderId fields

The `providerId` object represents the user for the current authentication method. Wasp passes it to the `onBeforeSignup`, `onAfterSignup`, `onBeforeLogin`, and `onAfterLogin` hooks.

It has the following fields:

- `providerName: string`

  The provider's name (e.g. `'email'`, `'google'`, `'github`)

- `providerUserId: string`

  The user's unique ID in the provider's system (e.g. email, Google ID, GitHub ID)

### OAuth fields

Wasp passes the `oauth` object to the `onAfterSignup` and `onAfterLogin` hooks only when the user is authenticated with [Social Auth](./social-auth/overview.md).

It has the following fields:

- `providerName: string`

  The name of the OAuth provider the user authenticated with (e.g. `'google'`, `'github'`).

- `tokens: Tokens`

  You can use the OAuth tokens to make requests to the provider's API on the user's behalf.

  Depending on the OAuth provider, the `tokens` object might have different fields. For example, Google has the fields `accessToken`, `refreshToken`, `idToken`, and `accessTokenExpiresAt`.

  <ShowForTs>

  To access the provider-specific fields, you must first narrow down the `oauth.tokens` object type to the specific OAuth provider type.

  ```ts
  if (oauth && oauth.providerName === 'google') {
    console.log(oauth.tokens.accessToken)
    //                  ^ Google specific tokens are available here
    console.log(oauth.tokens.refreshToken)
    console.log(oauth.tokens.idToken)
    console.log(oauth.tokens.accessTokenExpiresAt)
  }
  ```

  </ShowForTs>

- `uniqueRequestId: string`

  The unique request ID for the OAuth flow (you might know it as the `state` parameter in OAuth.)

  You can use the unique request ID to get the data that was saved in the `onBeforeOAuthRedirect` hook.
