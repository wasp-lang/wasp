---
title: Overview
---

import { SocialAuthGrid } from './SocialAuthGrid';
import DefaultBehaviour from './\_default-behaviour.md';
import OverrideIntro from './\_override-intro.md';
import GetUserFieldsType from './\_getuserfields-type.md';

Social login options (e.g., _Log in with Google_) are a great (maybe even the best) solution for handling user accounts.
A famous old developer joke tells us _"The best auth system is the one you never have to make."_

Wasp wants to make adding social login options to your app as painless as possible.

Using different social providers gives users a chance to sign into your app via their existing accounts on other platforms (Google, GitHub, etc.).

This page goes through the common behaviors between all supported social login providers and shows you how to customize them.
It also gives an overview of Wasp's UI helpers - the quickest possible way to get started with social auth.

## Available Providers

Wasp currently supports the following social login providers:

<SocialAuthGrid />

## Social Login Entity

Wasp requires you to declare a `userEntity` for all `auth` methods (social or otherwise).
This field tells Wasp which Entity represents the user.

Additionally, when using `auth` methods that rely on external providers(e.g., _Google_), you must also declare an `externalAuthEntity`.
This tells Wasp which Entity represents the user's link with the social provider.

Both fields fall under `app.auth`. Here's what the full setup looks like:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title=main.wasp
app myApp {
  wasp: {
    version: "^0.11.0"
  },
  title: "My App",
  auth: {
    // highlight-next-line
    userEntity: User,
    // highlight-next-line
    externalAuthEntity: SocialLogin,
    methods: {
      google: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
}

// highlight-next-line
entity User {=psl
    id                        Int           @id @default(autoincrement())
    //...
    externalAuthAssociations  SocialLogin[]
psl=}

// highlight-next-line
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
    // highlight-next-line
    userEntity: User,
    // highlight-next-line
    externalAuthEntity: SocialLogin,
    methods: {
      google: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
}

// highlight-next-line
entity User {=psl
    id                        Int           @id @default(autoincrement())
    //...
    externalAuthAssociations  SocialLogin[]
psl=}

// highlight-next-line
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

<small>

To learn more about what the fields on these entities represent, look at the [API Reference](#api-reference).

</small>

:::note
Wasp uses the same `externalAuthEntity` for all social login providers (e.g. both GitHub and Google use the same entity).
:::

## Default Behavior

<DefaultBehaviour />

## Overrides

Wasp lets you override the default behavior. You can create custom setups, such as allowing users to define a custom username rather instead of getting a randomly generated one.

### Allowing User to Set Their Username

If you want to modify the signup flow (e.g., let users choose their own usernames), you will need to go through three steps:

1. The first step is adding a `isSignupComplete` property to your `User` Entity. This field will signals whether the user has completed the signup process.
2. The second step is overriding the default signup behavior.
3. The third step is implementing the rest of your signup flow and redirecting users where appropriate.

Let's go through both steps in more detail.

#### 1. Adding the `isSignupComplete` Field to the `User` Entity

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title=main.wasp
entity User {=psl
    id                        Int           @id @default(autoincrement())
    username                  String?       @unique
    // highlight-next-line
    isSignupComplete          Boolean       @default(false)
    externalAuthAssociations  SocialLogin[]
psl=}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title=main.wasp
entity User {=psl
    id                        Int           @id @default(autoincrement())
    username                  String?       @unique
    // highlight-next-line
    isSignupComplete          Boolean       @default(false)
    externalAuthAssociations  SocialLogin[]
psl=}
```

</TabItem>
</Tabs>

#### 2. Overriding the Default Behavior

Declare an import under `app.auth.methods.google.getUserFieldsFn` (the example assumes you're using Google):

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
        // highlight-next-line
        getUserFieldsFn: import { getUserFields } from "@server/auth/google.js"
      }
    },
    onAuthFailedRedirectTo: "/login"
  },
}

// ...
```

And implement the imported function.

```js title=src/server/auth/google.js
export const getUserFields = async (_context, _args) => {
  return {
    isSignupComplete: false,
  }
}
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
        // highlight-next-line
        getUserFieldsFn: import { getUserFields } from "@server/auth/google.js"
      }
    },
    onAuthFailedRedirectTo: "/login"
  },
}

// ...
```

And implement the imported function:

```ts title=src/server/auth/google.ts
import { GetUserFieldsFn } from '@wasp/types'

export const getUserFields: GetUserFieldsFn = async (_context, _args) => {
  return {
    isSignupComplete: false,
  }
}
```

<GetUserFieldsType />

</TabItem>
</Tabs>

#### 3. Showing the Correct State on the Client

You can query the user's `isSignupComplete` flag on the client with the [`useAuth()`](../../auth/overview) hook.
Depending on the flag's value, you can redirect users to the appropriate signup step.

For example:

1. When the user lands on the homepage, check the value of `user.isSignupComplete`.
2. If it's `false`, it means the user has started the signup process but hasn't yet chosen their username. Therefore, you can redirect them to `EditUserDetailsPage` where they can edit the `username` property.

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

The same general principle applies to more complex signup procedures, just change the boolean `isSignupComplete` property to a property like `currentSignupStep` that can hold more values.

</TabItem>
</Tabs>

### Using the User's Provider Account Details

Account details are provider-specific.
Each provider has their own rules for defining the `getUserFieldsFn` and `configFn` functions:

<SocialAuthGrid pagePart="#overrides" />

## UI Helpers

:::tip Use Auth UI
[Auth UI](../../auth/ui) is a common name for all high-level auth forms that come with Wasp.

These include fully functional auto-generated login and signup forms with working social login buttons.
If you're looking for the fastest way to get your auth up and running, that's where you should look.

The UI helpers described below are lower-level and are useful for creating your custom forms.
:::

Wasp provides sign-in buttons and URLs for each of the supported social login providers.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title=client/LoginPage.jsx
import {
  SignInButton as GoogleSignInButton,
  signInUrl as googleSignInUrl,
} from '@wasp/auth/helpers/Google'
import {
  SignInButton as GitHubSignInButton,
  signInUrl as gitHubSignInUrl,
} from '@wasp/auth/helpers/GitHub'

export const LoginPage = () => {
  return (
    <>
      <GoogleSignInButton />
      <GitHubSignInButton />
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
} from '@wasp/auth/helpers/Google'
import {
  SignInButton as GitHubSignInButton,
  signInUrl as gitHubSignInUrl,
} from '@wasp/auth/helpers/GitHub'

export const LoginPage = () => {
  return (
    <>
      <GoogleSignInButton />
      <GitHubSignInButton />
      {/* or */}
      <a href={googleSignInUrl}>Sign in with Google</a>
      <a href={gitHubSignInUrl}>Sign in with GitHub</a>
    </>
  )
}
```

</TabItem>
</Tabs>

If you need even more customization, you can create your custom components using `signInUrl`s.

## API Reference

### Fields in the `app.auth` Dictionary and Overrides

For more information on:

- Allowed fields in `app.auth`
- `getUserFields` and `configFn` functions

Check the provider-specific API References:

<SocialAuthGrid pagePart="#api-reference" />

### The `externalAuthEntity` and Its Fields

Using social login providers requires you to define _an External Auth Entity_ and declare it with the `app.auth.externalAuthEntity` field.
This Entity holds the data relevant to the social provider.
All social providers share the same Entity.

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

The Entity acting as `app.auth.externalAuthEntity` must include the following fields:

- `provider` - The provider's name (e.g. `google`, `github`, etc.).
- `providerId` - The user's ID on the provider's platform.
- `userId` - The user's ID on your platform (this references the `id` field from the Entity acting as `app.auth.userEntity`).
- `user` - A relation to the `userEntity` (see [the `userEntity` section](#expected-fields-on-the-userentity)) for more details.
- `createdAt` - A timestamp of when the association was created.
- `@@unique([provider, providerId, userId])` - A unique constraint on the combination of `provider`, `providerId` and `userId`.

### Expected Fields on the `userEntity`

Using Social login providers requires you to add one extra field to the Entity acting as `app.auth.userEntity`:

- `externalAuthAssociations` - A relation to the `externalAuthEntity` (see [the `externalAuthEntity` section](#the-externalauthentity-and-its-fields) for more details).

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
