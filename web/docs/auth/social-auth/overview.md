---
title: Overview
title-llm: Social Auth Overview
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

## User Entity

Wasp requires you to declare a `userEntity` for all `auth` methods (social or otherwise).
This field tells Wasp which Entity represents the user.

Here's what the full setup looks like:

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "{latestWaspVersion}"
  },
  title: "My App",
  auth: {
    // highlight-next-line
    userEntity: User,
    methods: {
      google: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

```prisma title="schema.prisma"
// highlight-next-line
model User {
  id Int @id @default(autoincrement())
}
```

## Default Behavior

<DefaultBehaviour />

## Overrides

By default, Wasp doesn't store any information it receives from the social login provider. It only stores the user's ID specific to the provider.

If you wish to store more information about the user, you can override the default behavior. You can do this by defining the `userSignupFields` and `configFn` fields in `main.wasp` for each provider.

You can create custom signup setups, such as allowing users to define a custom username after they sign up with a social provider.

### Example: Allowing User to Set Their Username

If you want to modify the signup flow (e.g., let users choose their own usernames), you will need to go through three steps:

1. The first step is adding a `isSignupComplete` property to your `User` Entity. This field will signal whether the user has completed the signup process.
2. The second step is overriding the default signup behavior.
3. The third step is implementing the rest of your signup flow and redirecting users where appropriate.

Let's go through both steps in more detail.

#### 1. Adding the `isSignupComplete` Field to the `User` Entity

```prisma title="schema.prisma"
model User {
  id               Int     @id @default(autoincrement())
  username         String? @unique
  // highlight-next-line
  isSignupComplete Boolean @default(false)
}
```

#### 2. Overriding the Default Behavior

Declare an import under `app.auth.methods.google.userSignupFields` (the example assumes you're using Google):

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "{latestWaspVersion}"
  },
  title: "My App",
  auth: {
    userEntity: User,
    methods: {
      google: {
        // highlight-next-line
        userSignupFields: import { userSignupFields } from "@src/auth/google"
      }
    },
    onAuthFailedRedirectTo: "/login"
  },
}

// ...
```

And implement the imported function:

```ts title="src/auth/google.ts" auto-js
import { defineUserSignupFields } from 'wasp/server/auth'

export const userSignupFields = defineUserSignupFields({
  isSignupComplete: () => false,
})
```

<GetUserFieldsType />

#### 3. Showing the Correct State on the Client

You can check the `isSignupComplete` flag on the `user` object.
Authenticated pages come with the [`user` prop](../../auth/overview#getting-the-user-in-authenticated-routes) which gives you access to the current user. If the `user` prop is out of reach, fetch the current user with the  [`useAuth()` hook](../../auth/overview#getting-the-user-in-non-authenticated-routes).

Depending on the flag's value, you can redirect users to the appropriate signup step.

For example:

1. When the user lands on the homepage, check the value of `user.isSignupComplete`.
2. If it's `false`, it means the user has started the signup process but hasn't yet chosen their username. Therefore, you can redirect them to `EditUserDetailsPage` where they can edit the `username` property.

```tsx title="src/HomePage.tsx" auto-js
import { Navigate } from 'react-router-dom'
import type { AuthUser } from 'wasp/auth'

export function HomePage({ user }: { user: AuthUser }) {
  if (user.isSignupComplete === false) {
    return <Navigate to="/edit-user-details" />
  }

  // ...
}
```

The same general principle applies to more complex signup procedures, just change the boolean `isSignupComplete` property to a property like `currentSignupStep` that can hold more values.

### Using the User's Provider Account Details

Account details are provider-specific.
Each provider has their own rules for defining the `userSignupFields` and `configFn` fields:

<SocialAuthGrid pagePart="#overrides" />

## API Reference

### Fields in the `app.auth` Dictionary and Overrides

For more information on:

- Allowed fields in `app.auth`
- `userSignupFields` and `configFn` functions

Check the provider-specific API References:

<SocialAuthGrid pagePart="#api-reference" />
