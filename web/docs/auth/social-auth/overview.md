---
title: Overview
---

import { SocialAuthGrid } from './SocialAuthGrid';
import DefaultBehaviour from './_default-behaviour.md';
import OverrideIntro from './_override-intro.md';

With Wasp, adding social login options to your app is easy. By using social login providers, you allow your users to sign in to your app using their existing accounts on other platforms, such as Google, GitHub, etc.

In this section, we'll take a look at some of the common behaviors between all social login providers Wasp supports and how to customize them. We'll also talk about the UI helpers which make it easier to add social login to your app.

## Available Providers

Wasp currently supports the following social login providers:

<SocialAuthGrid />

## Social Login Entity

When you use any type of `auth` you need to define the `userEntity` which is the entity that represents your users.

Additionally, when you use an `auth` method that relies on an external provider, for example, **Google**, you need to define the `externalAuthEntity`.

The setup looks like this:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title=main.wasp
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

```wasp title=main.wasp
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
The same `externalAuthEntity` is used for different social login providers (e.g. both GitHub and Google can use the same entity).
:::

## Default Behavior

<DefaultBehaviour />

## Overrides

It is also possible to override the default behavior that Wasp provides for you. This allows you to create custom setups, such as allowing users to define a username rather than the default random username on the initial login.

###  Allowing User to Set Their Username

If you would like to allow the user to select their username or some other sign-up flow, you could add an `isSignupComplete` property to your `User` entity indicating the account setup is incomplete. Then, we would override the default username behavior.

#### 1. Add `isSignupComplete` to `User` Entity

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title=main.wasp
app myApp {
  wasp: {
    version: "^0.11.0"
  },
  title: "My App",
  auth: {
    userEntity: User,
    externalAuthEntity: SocialLogin,
    methods: {
      google: {
        getUserFieldsFn: import { getUserFields } from "@server/auth/google.js"
      }
    },
    onAuthFailedRedirectTo: "/login"
  },
}

entity User {=psl
    id                        Int           @id @default(autoincrement())
    username                  String?       @unique
    isSignupComplete          Boolean       @default(false)
    externalAuthAssociations  SocialLogin[]
psl=}

// ...
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title=main.wasp
app myApp {
  wasp: {
    version: "^0.11.0"
  },
  title: "My App",
  auth: {
    userEntity: User,
    externalAuthEntity: SocialLogin,
    methods: {
      google: {
        getUserFieldsFn: import { getUserFields } from "@server/auth/google.js"
      }
    },
    onAuthFailedRedirectTo: "/login"
  },
}

entity User {=psl
    id                        Int           @id @default(autoincrement())
    username                  String?       @unique
    isSignupComplete          Boolean       @default(false)
    externalAuthAssociations  SocialLogin[]
psl=}

// ...
```
</TabItem>
</Tabs>


#### 2. Override the Default Behavior

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title=src/server/auth/google.js
export const getUserFields = async (_context, _args) => {
  return {
    isSignupComplete: false,
  }
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title=src/server/auth/google.ts
import { GetUserFieldsFn } from '@wasp/types';

export const getUserFields: GetUserFieldsFn = async (_context, _args) => {
  return {
    isSignupComplete: false,
  }
}
```
</TabItem>
</Tabs>

#### 3. Check the User's Property on the Client

You can then check this user's property on the client with the [`useAuth()`](/docs/auth/overview) hook and redirect them when appropriate.

For example, check on the homepage if `user.isSignupComplete === false`, and redirect them to `EditUserDetailsPage` where they can edit the `username` property.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title=client/HomePage.jsx
import useAuth from '@wasp/auth/useAuth'
import { Redirect } from 'react-router-dom'

export function HomePage() {
  const { data: user } = useAuth()

  if (user.isSignupComplete === false) {
    return <Redirect to="/edit-user-details" />
  }

  // ...
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title=client/HomePage.tsx
import useAuth from '@wasp/auth/useAuth'
import { Redirect } from 'react-router-dom'

export function HomePage() {
  const { data: user } = useAuth()

  if (user.isSignupComplete === false) {
    return <Redirect to="/edit-user-details" />
  }

  // ...
}
```
</TabItem>
</Tabs>

### Using the User's Provider Account Details

Check out the examples for different providers to see how to use `getUserFieldsFn` and `configFn` functions:

<SocialAuthGrid pagePart="#overrides" />

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

### `userEntity` fields

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title=main.wasp {6}
// ...

entity User {=psl
    id                        Int           @id @default(autoincrement())
    //...
    externalAuthAssociations  SocialLogin[]
psl=}

// ...
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title=main.wasp {6}
// ...

entity User {=psl
    id                        Int           @id @default(autoincrement())
    //...
    externalAuthAssociations  SocialLogin[]
psl=}

// ...
```
</TabItem>
</Tabs>

To use social login providers, the `userEntity` must have the following fields:
- `externalAuthAssociations` - a relation to the `externalAuthEntity` (see below)

### `externalAuthEntity` fields

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title=main.wasp {4-10}
// ...

entity SocialLogin {=psl
  id          Int       @id @default(autoincrement())
  provider    String
  providerId  String
  user        User      @relation(fields: [userId], references: [id], onDelete: Cascade)
  userId      Int
  createdAt   DateTime  @default(now())
  @@unique([provider, providerId, userId])
psl=}

// ...
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title=main.wasp {4-10}
// ...

entity SocialLogin {=psl
  id          Int       @id @default(autoincrement())
  provider    String
  providerId  String
  user        User      @relation(fields: [userId], references: [id], onDelete: Cascade)
  userId      Int
  createdAt   DateTime  @default(now())
  @@unique([provider, providerId, userId])
psl=}

// ...
```
</TabItem>
</Tabs>

:::info
You don't need to know these details, you can just copy and paste the entity definition above and you are good to go.
:::

The `externalAuthEntity` must have the following fields:
- `provider` - the name of the provider (e.g. `google`, `github`, etc.)
- `providerId` - the ID of the user on the provider's platform
- `userId` - the ID of the user on your platform
- `user` - a relation to the `userEntity` (see above)
- `createdAt` - a timestamp of when the association was created
- `@@unique([provider, providerId, userId])` - a unique constraint on the combination of `provider`, `providerId` and `userId`
