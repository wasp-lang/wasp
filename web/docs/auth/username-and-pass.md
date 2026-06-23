---
title: Overview
title-llm: Username & Password Auth Overview
---

import { CardLink } from '@site/src/components/CardLink';
import MultipleIdentitiesWarning from './\_multiple-identities-warning.md';
import ReadMoreAboutAuthEntities from './\_read-more-about-auth-entities.md';
import UsernameData from './entities/\_username-data.md';
import AccessingUserDataNote from './\_accessing-user-data-note.md';
import TailwindNote from './\_tailwind-note.md';

Wasp supports username & password authentication out of the box with login and signup flows. It provides you with the server-side implementation and the UI components for the client side.

## Setting Up Username & Password Authentication

To set up username authentication we need to:

1. Enable username authentication in the Wasp file
2. Add the `User` entity
3. Add the auth routes and pages
4. Use Auth UI components in our pages

Structure of the `main.wasp.ts` file we will end up with:

```ts title="main.wasp.ts"
import { app, page, route } from "@wasp.sh/spec"
import { SignupPage } from "./src/pages/auth" with { type: "ref" }

// Configuring e-mail authentication
export default app({
  name: "myApp",
  wasp: { version: "{latestWaspVersion}" },
  title: "My App",
  head: ["<link rel='icon' href='/favicon.ico' />"],
  auth: {
    // ...
  },
  spec: [
    // Defining routes and pages
    route("SignupRoute", "/signup", page(SignupPage)),
    // ...
  ],
})
```

### 1. Enable Username Authentication

Let's start with adding the following to our `main.wasp.ts` file:

```ts title="main.wasp.ts" {12}
import { app } from "@wasp.sh/spec"

export default app({
  name: "myApp",
  wasp: { version: "{latestWaspVersion}" },
  title: "My App",
  head: ["<link rel='icon' href='/favicon.ico' />"],
  auth: {
    // 1. Specify the user entity (we'll define it next)
    userEntity: "User",
    methods: {
      // 2. Enable username authentication
      usernameAndPassword: {},
    },
    onAuthFailedRedirectTo: "/login"
  },
  // ...
})
```

Read more about the `usernameAndPassword` auth method options in the [`UsernameAndPasswordConfig` API Reference](../api/@wasp.sh/spec/interfaces/UsernameAndPasswordConfig.md).

### 2. Add the User Entity

The `User` entity can be as simple as including only the `id` field:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```prisma title="schema.prisma"
    // 3. Define the user entity
    model User {
      // highlight-next-line
      id Int @id @default(autoincrement())
      // Add your own fields below
      // ...
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```prisma title="schema.prisma"
    // 3. Define the user entity
    model User {
      // highlight-next-line
      id Int @id @default(autoincrement())
      // Add your own fields below
      // ...
    }
    ```
  </TabItem>
</Tabs>

<ReadMoreAboutAuthEntities />

### 3. Add the Routes and Pages

Next, we need to define the routes and pages for the authentication pages.

Add the following to the `main.wasp.ts` file:

```ts title="main.wasp.ts"
import { app, page, route } from "@wasp.sh/spec"
import { LoginPage, SignupPage } from "./src/pages/auth" with { type: "ref" }

export default app({
  // ...
  spec: [
    route("LoginRoute", "/login", page(LoginPage)),
    route("SignupRoute", "/signup", page(SignupPage)),
  ],
})
```

We'll define the React components for these pages in the `src/pages/auth.{jsx,tsx}` file below.

### 4. Create the Client Pages

Copy the drop-in login and signup page code from the [Auth UI docs](../auth/ui#drop-in-custom-auth-ui). The copied UI uses `passwordAuthIdentityField` from `wasp/client/auth`, so it renders a username field for this auth method.

### Conclusion

That's it! We have set up username authentication in our app. 🎉

Running `wasp db migrate-dev` and then `wasp start` should give you a working app with username authentication. If you want to put some of the pages behind authentication, read the [auth overview docs](../auth/overview).

<MultipleIdentitiesWarning />

## Using Auth

To read more about how to set up the logout button and how to get access to the logged-in user in our client and server code, read the [auth overview docs](../auth/overview).

When you receive the `user` object [on the client or the server](./overview.md#accessing-the-logged-in-user), you'll be able to access the user's username like this:

<UsernameData />

<AccessingUserDataNote />

## API Reference

<CardLink
  to="../api/@wasp.sh/spec/interfaces/Auth"
  kind="api"
  title="Auth"
  description="All the options for the auth field of the app spec, including userEntity."
/>

<CardLink
  to="../api/@wasp.sh/spec/interfaces/UsernameAndPasswordConfig"
  kind="api"
  title="UsernameAndPasswordConfig"
  description="All the options for the usernameAndPassword auth method."
/>

Read more about the `userSignupFields` function in the [Auth Overview docs](./overview.md#signup-fields-customization).
