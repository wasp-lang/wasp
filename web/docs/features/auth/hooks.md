---
title: "Auth hooks: run custom code on signup and login"
description: Run custom code at stages of the Wasp auth flow with hooks like onBeforeSignup and onAfterLogin, to block certain emails or send a welcome email.
sidebar_label: Hooks
---

import { EmailPill, UsernameAndPasswordPill, GithubPill, GooglePill, KeycloakPill, SlackPill, DiscordPill } from "./Pills";
import { ImgWithCaption } from '@site/blog/components/ImgWithCaption'
import { CardLink } from '@site/src/components/CardLink'

# Auth hooks

Auth hooks allow you to "hook into" the auth process at various stages and run your custom code. For example, if you want to forbid certain emails from signing up, or if you wish to send a welcome email to the user after they sign up, auth hooks are the way to go.

## Supported hooks

The following auth hooks are available in Wasp:

- [`onBeforeSignup`](#executing-code-before-the-user-signs-up)
- [`onAfterSignup`](#executing-code-after-the-user-signs-up)
- [`onAfterEmailVerified`](#executing-code-after-a-user-verifies-their-email)
- [`onBeforeOAuthRedirect`](#executing-code-before-the-oauth-redirect)
- [`onBeforeLogin`](#executing-code-before-the-user-logs-in)
- [`onAfterLogin`](#executing-code-after-the-user-logs-in)

We'll go through each of these hooks in detail. But first, let's see how the hooks fit into the auth flows:

<ImgWithCaption source="/img/docs/features/auth/hooks/signup_flow_with_hooks.png" alt="Signup Flow with Hooks" caption="Signup Flow with Hooks" />

<ImgWithCaption source="/img/docs/features/auth/hooks/login_flow_with_hooks.png" alt="Login Flow with Hooks" caption="Login Flow with Hooks *" />

<small>
  \* When using the OAuth auth providers, the login hooks are both called before the session is created but the session is created quickly afterward, so it shouldn't make any difference in practice.
</small>

Users registering with [email](./email/overview.md) must verify it before they can log in. This verification triggers the Email verification flow:

<ImgWithCaption
  source="/img/docs/features/auth/hooks/email_verification_flow_with_hooks.png"
  alt="Email Verification Flow with Hooks"
  caption="Email Verification Flow with Hooks"
/>

Users signing in with [OAuth](./social-auth/overview.md) must authorize access before completing login. This authorization triggers the OAuth consent flow:

<ImgWithCaption source="/img/docs/features/auth/hooks/oauth_flow_with_hooks.png" alt="OAuth Flow with Hooks" caption="OAuth Flow with Hooks" />

## Using hooks

To use auth hooks, you must first declare them in the Wasp file:

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"
import {
  onBeforeSignup,
  onAfterSignup,
  onAfterEmailVerified,
  onBeforeOAuthRedirect,
  onBeforeLogin,
  onAfterLogin,
} from "./src/auth/hooks" with { type: "ref" }

export default app({
  name: "myApp",
  wasp: { version: "{latestWaspVersion}" },
  title: "My App",
  head: ["<link rel='icon' href='/favicon.ico' />"],
  auth: {
    userEntity: "User",
    methods: {
      // ...
    },
    onBeforeSignup,
    onAfterSignup,
    onAfterEmailVerified,
    onBeforeOAuthRedirect,
    onBeforeLogin,
    onAfterLogin,
  },
  // ...
})
```

If the hooks are defined as async functions, Wasp _awaits_ them. This means the auth process waits for the hooks to finish before continuing.

Wasp ignores the hooks' return values. The only exception is the `onBeforeOAuthRedirect` hook, whose return value affects the OAuth redirect URL.

We'll now go through each of the available hooks.

### Executing code before the user signs up

Wasp calls the `onBeforeSignup` hook before the user is created.

The `onBeforeSignup` hook can be useful if you want to reject a user based on some criteria before they sign up.

Works with <EmailPill /> <UsernameAndPasswordPill /> <SlackPill /> <DiscordPill /> <GithubPill /> <GooglePill /> <KeycloakPill />

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"
import { onBeforeSignup } from "./src/auth/hooks" with { type: "ref" }

export default app({
  // ...
  auth: {
    // ...
    onBeforeSignup,
  },
  // ...
})
```

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="src/auth/hooks.js"
    import { HttpError } from "wasp/server"

    export const onBeforeSignup = async ({ providerId, prisma, req }) => {
      const count = await prisma.user.count()
      console.log("number of users before", count)
      console.log("provider name", providerId.providerName)
      console.log("provider user ID", providerId.providerUserId)

      if (count > 100) {
        throw new HttpError(403, "Too many users")
      }

      if (
        providerId.providerName === "email" &&
        providerId.providerUserId === "some@email.com"
      ) {
        throw new HttpError(403, "This email is not allowed")
      }
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="src/auth/hooks.ts"
    import { HttpError } from "wasp/server"
    import type { OnBeforeSignupHook } from "wasp/server/auth"

    export const onBeforeSignup: OnBeforeSignupHook = async ({
      providerId,
      prisma,
      req,
    }) => {
      const count = await prisma.user.count()
      console.log("number of users before", count)
      console.log("provider name", providerId.providerName)
      console.log("provider user ID", providerId.providerUserId)

      if (count > 100) {
        throw new HttpError(403, "Too many users")
      }

      if (
        providerId.providerName === "email" &&
        providerId.providerUserId === "some@email.com"
      ) {
        throw new HttpError(403, "This email is not allowed")
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

Works with <EmailPill /> <UsernameAndPasswordPill /> <SlackPill /> <DiscordPill /> <GithubPill /> <GooglePill /> <KeycloakPill />

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"
import { onAfterSignup } from "./src/auth/hooks" with { type: "ref" }

export default app({
  // ...
  auth: {
    // ...
    onAfterSignup,
  },
  // ...
})
```

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
      const count = await prisma.user.count()
      console.log("number of users after", count)
      console.log("user object", user)

      // If this is an OAuth signup, you have access to the OAuth tokens and the uniqueRequestId
      if (oauth) {
        console.log("accessToken", oauth.tokens.accessToken)
        console.log("uniqueRequestId", oauth.uniqueRequestId)

        const id = oauth.uniqueRequestId
        const data = someKindOfStore.get(id)
        if (data) {
          console.log("saved data for the ID", data)
        }
        someKindOfStore.delete(id)
      }
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="src/auth/hooks.ts"
    import type { OnAfterSignupHook } from "wasp/server/auth"

    export const onAfterSignup: OnAfterSignupHook = async ({
      providerId,
      user,
      oauth,
      prisma,
      req,
    }) => {
      const count = await prisma.user.count()
      console.log("number of users after", count)
      console.log("user object", user)

      // If this is an OAuth signup, you have access to the OAuth tokens and the uniqueRequestId
      if (oauth) {
        console.log("accessToken", oauth.tokens.accessToken)
        console.log("uniqueRequestId", oauth.uniqueRequestId)

        const id = oauth.uniqueRequestId
        const data = someKindOfStore.get(id)
        if (data) {
          console.log("saved data for the ID", data)
        }
        someKindOfStore.delete(id)
      }
    }
    ```
  </TabItem>
</Tabs>

Read more about the data the `onAfterSignup` hook receives in the [API Reference](#the-onaftersignup-hook).

### Executing code after a user verifies their email

Wasp calls the `onAfterEmailVerified` hook exactly once, after the user verifies their email.

The `onAfterEmailVerified` hook is useful for triggering actions in response to the verification event — such as sending a welcome email or syncing user data with a third-party service.

The `onAfterEmailVerified` hook receives an `email` string and `user` object, this makes it easy to perform personalized actions upon email verification.

Works with <EmailPill />

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"
import { onAfterEmailVerified } from "./src/auth/hooks" with { type: "ref" }

export default app({
  // ...
  auth: {
    // ...
    onAfterEmailVerified,
  },
  // ...
})
```

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/auth/hooks.js"
import { emailSender } from "wasp/server/email"

export const onAfterEmailVerified = async ({ email }) => {
  const info = await emailSender.send({
    from: {
      name: "John Doe",
      email: "john@doe.com",
    },
    to: email,
    subject: "Thank you for verifying your email!",
    text: `Your email ${email} has been successfully verified!`,
  })
  // ...
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/auth/hooks.ts"
import type { OnAfterEmailVerifiedHook } from "wasp/server/auth"
import { emailSender } from "wasp/server/email"

export const onAfterEmailVerified: OnAfterEmailVerifiedHook = async ({
  email,
}) => {
  const info = await emailSender.send({
    from: {
      name: "John Doe",
      email: "john@doe.com",
    },
    to: email,
    subject: "Thank you for verifying your email!",
    text: `Your email ${email} has been successfully verified!`,
  })
  // ...
}
```

</TabItem>
</Tabs>

Read more about the data the `onAfterEmailVerified` hook receives in the [API Reference](#the-onafteremailverified-hook).

### Executing code before the OAuth redirect

Wasp calls the `onBeforeOAuthRedirect` hook after the OAuth redirect URL is generated but before redirecting the user. This hook can access the request object sent from the client at the start of the OAuth process.

The `onBeforeOAuthRedirect` hook can be useful if you want to save some data (e.g. request query parameters) that you can use later in the OAuth flow. You can use the `uniqueRequestId` parameter to reference this data later in the `onAfterSignup` or `onAfterLogin` hooks.

Works with <DiscordPill /> <GithubPill /> <GooglePill /> <KeycloakPill />

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"
import { onBeforeOAuthRedirect } from "./src/auth/hooks" with { type: "ref" }

export default app({
  // ...
  auth: {
    // ...
    onBeforeOAuthRedirect,
  },
  // ...
})
```

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="src/auth/hooks.js"
    export const onBeforeOAuthRedirect = async ({ url, oauth, prisma, req }) => {
      console.log("query params before oAuth redirect", req.query)

      // Saving query params for later use in onAfterSignup or onAfterLogin hooks
      const id = oauth.uniqueRequestId
      someKindOfStore.set(id, req.query)

      return { url }
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="src/auth/hooks.ts"
    import type { OnBeforeOAuthRedirectHook } from "wasp/server/auth"

    export const onBeforeOAuthRedirect: OnBeforeOAuthRedirectHook = async ({
      url,
      oauth,
      prisma,
      req,
    }) => {
      console.log("query params before oAuth redirect", req.query)

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

Works with <EmailPill /> <UsernameAndPasswordPill /> <SlackPill /> <DiscordPill /> <GithubPill /> <GooglePill /> <KeycloakPill />

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"
import { onBeforeLogin } from "./src/auth/hooks" with { type: "ref" }

export default app({
  // ...
  auth: {
    // ...
    onBeforeLogin,
  },
  // ...
})
```

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="src/auth/hooks.js"
    import { HttpError } from "wasp/server"

    export const onBeforeLogin = async ({ providerId, user, prisma, req }) => {
      if (
        providerId.providerName === "email" &&
        providerId.providerUserId === "some@email.com"
      ) {
        throw new HttpError(403, "You cannot log in with this email")
      }
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="src/auth/hooks.ts"
    import { HttpError } from "wasp/server"
    import type { OnBeforeLoginHook } from "wasp/server/auth"

    export const onBeforeLogin: OnBeforeLoginHook = async ({
      providerId,
      user,
      prisma,
      req,
    }) => {
      if (
        providerId.providerName === "email" &&
        providerId.providerUserId === "some@email.com"
      ) {
        throw new HttpError(403, "You cannot log in with this email")
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

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"
import { onAfterLogin } from "./src/auth/hooks" with { type: "ref" }

export default app({
  // ...
  auth: {
    // ...
    onAfterLogin,
  },
  // ...
})
```

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
      console.log("user object", user)

      // If this is an OAuth signup, you have access to the OAuth tokens and the uniqueRequestId
      if (oauth) {
        console.log("accessToken", oauth.tokens.accessToken)
        console.log("uniqueRequestId", oauth.uniqueRequestId)

        const id = oauth.uniqueRequestId
        const data = someKindOfStore.get(id)
        if (data) {
          console.log("saved data for the ID", data)
        }
        someKindOfStore.delete(id)
      }
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="src/auth/hooks.ts"
    import type { OnAfterLoginHook } from "wasp/server/auth"

    export const onAfterLogin: OnAfterLoginHook = async ({
      providerId,
      user,
      oauth,
      prisma,
      req,
    }) => {
      console.log("user object", user)

      // If this is an OAuth signup, you have access to the OAuth tokens and the uniqueRequestId
      if (oauth) {
        console.log("accessToken", oauth.tokens.accessToken)
        console.log("uniqueRequestId", oauth.uniqueRequestId)

        const id = oauth.uniqueRequestId
        const data = someKindOfStore.get(id)
        if (data) {
          console.log("saved data for the ID", data)
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
    import { google } from "wasp/server/auth"

    export const onAfterLogin = async ({ oauth }) => {
      if (oauth.provider === "google" && oauth.tokens.refreshToken !== null) {
        const newTokens = await google.oAuthClient.refreshAccessToken(
          oauth.tokens.refreshToken
        )
        log("new tokens", newTokens)
      }
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="src/auth/hooks.ts"
    import type { OnAfterLoginHook } from "wasp/server/auth"
    import { google } from "wasp/server/auth"

    export const onAfterLogin: OnAfterLoginHook = async ({ oauth }) => {
      if (oauth.provider === "google" && oauth.tokens.refreshToken !== null) {
        const newTokens = await google.oAuthClient.refreshAccessToken(
          oauth.tokens.refreshToken
        )
        log("new tokens", newTokens)
      }
    }
    ```
  </TabItem>
</Tabs>

Google exposes the `accessTokenExpiresAt` field in the `oauth.tokens` object. You can use this field to determine when the access token expires.

If you want to refresh the token periodically, use a [Wasp Job](../jobs.md).

## API Reference

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"
import {
  onBeforeSignup,
  onAfterSignup,
  onAfterEmailVerified,
  onBeforeOAuthRedirect,
  onBeforeLogin,
  onAfterLogin,
} from "./src/auth/hooks" with { type: "ref" }

export default app({
  name: "myApp",
  wasp: { version: "{latestWaspVersion}" },
  title: "My App",
  head: ["<link rel='icon' href='/favicon.ico' />"],
  auth: {
    userEntity: "User",
    methods: {
      // ...
    },
    onBeforeSignup,
    onAfterSignup,
    onAfterEmailVerified,
    onBeforeOAuthRedirect,
    onBeforeLogin,
    onAfterLogin,
  },
  // ...
})
```

### The `onBeforeSignup` hook

<CardLink
  to="../../api/wasp/server/auth/type-aliases/OnBeforeSignupHook"
  kind="api"
  title="OnBeforeSignupHook"
  description="The type of the onBeforeSignup hook, and the input it receives."
/>

### The `onAfterSignup` hook

<CardLink
  to="../../api/wasp/server/auth/type-aliases/OnAfterSignupHook"
  kind="api"
  title="OnAfterSignupHook"
  description="The type of the onAfterSignup hook, and the input it receives."
/>

### The `onAfterEmailVerified` hook

<CardLink
  to="../../api/wasp/server/auth/type-aliases/OnAfterEmailVerifiedHook"
  kind="api"
  title="OnAfterEmailVerifiedHook"
  description="The type of the onAfterEmailVerified hook, and the input it receives."
/>

### The `onBeforeOAuthRedirect` hook

<CardLink
  to="../../api/wasp/server/auth/type-aliases/OnBeforeOAuthRedirectHook"
  kind="api"
  title="OnBeforeOAuthRedirectHook"
  description="The type of the onBeforeOAuthRedirect hook, the input it receives, and what it returns."
/>

### The `onBeforeLogin` hook

<CardLink
  to="../../api/wasp/server/auth/type-aliases/OnBeforeLoginHook"
  kind="api"
  title="OnBeforeLoginHook"
  description="The type of the onBeforeLogin hook, and the input it receives."
/>

### The `onAfterLogin` hook

<CardLink
  to="../../api/wasp/server/auth/type-aliases/OnAfterLoginHook"
  kind="api"
  title="OnAfterLoginHook"
  description="The type of the onAfterLogin hook, and the input it receives."
/>

### ProviderId fields

<CardLink
  to="../../api/wasp/server/auth/type-aliases/ProviderId"
  kind="api"
  title="ProviderId"
  description="The object that identifies the user for the current auth method."
/>

### OAuth fields

<CardLink
  to="../../api/wasp/server/auth/type-aliases/OAuthData"
  kind="api"
  title="OAuthData"
  description="The OAuth flow data, including the OAuth tokens."
/>
