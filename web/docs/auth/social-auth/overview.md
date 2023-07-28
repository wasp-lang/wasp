---
title: Overview
---

import { SocialAuthGrid } from './SocialAuthGrid';

Wasp allows you to easily add social login providers to your app.

When using Social Login Providers, Wasp gives you the following options:
- Default settings to get you started quickly
- UI Helpers to make it easy to add social login buttons and actions
- Override settings to customize the behavior of the providers

## Available Providers

Wasp currently supports the following social login providers:

<SocialAuthGrid />

## Social Login Entity
When an authentication method is used that relies on an external authorization provider, for example, **Google**, we require an `externalAuthEntity` specified in `auth`, in addition to the `userEntity`, that contains the following configuration:


<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp
app myApp {
  wasp: {
    version: "^0.11.0"
  },
  title: "My App",
  auth: {
    userEntity: User,
    externalAuthEntity: SocialLogin,
    methods: {
      google: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
}

entity User {=psl
    id                        Int           @id @default(autoincrement())
    //...
    externalAuthAssociations  SocialLogin[]
psl=}

entity SocialLogin {=psl
  id          Int       @id @default(autoincrement())
  provider    String
  providerId  String
  user        User      @relation(fields: [userId], references: [id], onDelete: Cascade)
  userId      Int
  createdAt   DateTime  @default(now())
  @@unique([provider, providerId, userId])
psl=}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp
app myApp {
  wasp: {
    version: "^0.11.0"
  },
  title: "My App",
  auth: {
    userEntity: User,
    externalAuthEntity: SocialLogin,
    methods: {
      google: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
}

entity User {=psl
    id                        Int           @id @default(autoincrement())
    //...
    externalAuthAssociations  SocialLogin[]
psl=}

entity SocialLogin {=psl
  id          Int       @id @default(autoincrement())
  provider    String
  providerId  String
  user        User      @relation(fields: [userId], references: [id], onDelete: Cascade)
  userId      Int
  createdAt   DateTime  @default(now())
  @@unique([provider, providerId, userId])
psl=}
```
</TabItem>
</Tabs>

:::note
The same `externalAuthEntity` can be used across different social login providers (e.g. both GitHub and Google can use the same entity).
:::

## Default Settings

When a user signs in for the first time, if the `userEntity` has `username` and/or `password` fields Wasp assigns generated values to those fields by default (e.g. `username: nice-blue-horse-14357` and a strong random `password`). 

This is a historical coupling between auth methods that will be removed over time.

:::info Overriding Defaults
It is also possible to override the default login behaviors that Wasp provides for you. This allows you to create custom setups, such as allowing users to define a username rather than the default random username assigned by Wasp on initial Login.
:::

## Overrides

When a user signs in for the first time, Wasp will create a new user account and link it to the chosen Auth Provider account for future logins. If the `userEntity` contains a `username` field it will default to a random dictionary phrase that does not exist in the database, such as `nice-blue-horse-27160`. This is a historical coupling between auth methods that will be removed over time.

###  Example of Customizing the Flow

If you would like to allow the user to select their username, or some other sign up flow, you could add a boolean property to your `User` entity indicating the account setup is incomplete. You can then check this user's property on the client with the [`useAuth()`](#useauth) hook and redirect them when appropriate
  - e.g. check on the homepage if `user.isAuthSetup === false`, redirect them to `EditUserDetailsPage` where they can edit the `username` property.

### Setting Custom Username

Alternatively, you could add a `displayName` property to your User entity and assign it using the details of their provider account. Below is an example of how to do this by using:
  - the `getUserFieldsFn` function to configure the user's `username` or `displayName` from their provider account

We also show you how to customize the configuration of the Provider's settings using:
  - the `configFn` function

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title=main.wasp {9,10,13,14,26}
app Example {
  //...

  auth: {
    userEntity: User,
    externalAuthEntity: SocialLogin,
    methods: {
      google: {
        configFn: import { config } from "@server/auth/google.js",
        getUserFieldsFn: import { getUserFields } from "@server/auth/google.js"
      },
      gitHub: {
        configFn: import { config } from "@server/auth/github.js",
        getUserFieldsFn: import { getUserFields } from "@server/auth/github.js"
      }
    },

   //...
  }
}

entity User {=psl
    id          Int     @id @default(autoincrement())
    username    String  @unique
    password    String
    displayName String?
    externalAuthAssociations  SocialLogin[]
psl=}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title=main.wasp {9,10,13,14,26}
app Example {
  //...

  auth: {
    userEntity: User,
    externalAuthEntity: SocialLogin,
    methods: {
      google: {
        configFn: import { config } from "@server/auth/google.js",
        getUserFieldsFn: import { getUserFields } from "@server/auth/google.js"
      },
      gitHub: {
        configFn: import { config } from "@server/auth/github.js",
        getUserFieldsFn: import { getUserFields } from "@server/auth/github.js"
      }
    },

   //...
  }
}

entity User {=psl
    id          Int     @id @default(autoincrement())
    username    String  @unique
    password    String
    displayName String?
    externalAuthAssociations  SocialLogin[]
psl=}
```
</TabItem>
</Tabs>

## UI helpers

:::tip Use Auth UI
You can use the higher-level forms provided by Wasp called [Auth UI](/docs/auth/ui). It provides you with auto-generated forms for login and signup which already include the social login buttons.

The UI helpers described below are lower-level and are useful if you want to create your custom forms.
:::

Wasp provides sign-in buttons, logos and URLs for each of the supported social login providers.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title=client/LoginPage.jsx
import {
  SignInButton as GoogleSignInButton,
  signInUrl as googleSignInUrl,
  logoUrl as googleLogoUrl,
} from '@wasp/auth/helpers/Google'
import {
  SignInButton as GitHubSignInButton,
  signInUrl as gitHubSignInUrl,
  logoUrl as gitHubLogoUrl,
} from '@wasp/auth/helpers/GitHub'

export const LoginPage = () => {
  return (
    <>
      <GoogleSignInButton/>
      <GitHubSignInButton/>
      {/* or */}
      <a href={googleSignInUrl}>Sign in with Google</a>
      <a href={gitHubSignInUrl}>Sign in with GitHub</a>
    </>
  )
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title=client/LoginPage.tsx
import {
  SignInButton as GoogleSignInButton,
  signInUrl as googleSignInUrl,
  logoUrl as googleLogoUrl,
} from '@wasp/auth/helpers/Google'
import {
  SignInButton as GitHubSignInButton,
  signInUrl as gitHubSignInUrl,
  logoUrl as gitHubLogoUrl,
} from '@wasp/auth/helpers/GitHub'

export const LoginPage = () => {
  return (
    <>
      <GoogleSignInButton/>
      <GitHubSignInButton/>
      {/* or */}
      <a href={googleSignInUrl}>Sign in with Google</a>
      <a href={gitHubSignInUrl}>Sign in with GitHub</a>
    </>
  )
}
```
</TabItem>
</Tabs>

If you need more customization than what the buttons provide, you can create your custom components using the `signInUrl`s.

## Options Reference

TODO: write about the `externalAuthEntity` and `userEntity` fields in the `auth` object in `main.wasp`.