---
title: Overview
title-llm: Username & Password Auth Overview
---

import { Required } from '@site/src/components/Tag';
import MultipleIdentitiesWarning from './\_multiple-identities-warning.md';
import ReadMoreAboutAuthEntities from './\_read-more-about-auth-entities.md';
import UserSignupFieldsExplainer from './\_user-signup-fields-explainer.md';
import UserFieldsExplainer from './\_user-fields.md';
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
  auth: {
    // ...
  },
  decls: [
    // Defining routes and pages
    route("SignupRoute", "/signup", page(SignupPage)),
    // ...
  ],
})
```

### 1. Enable Username Authentication

Let's start with adding the following to our `main.wasp.ts` file:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```ts title="main.wasp.ts" {12}
    import { app } from "@wasp.sh/spec"

    export default app({
      name: "myApp",
      wasp: { version: "{latestWaspVersion}" },
      title: "My App",
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
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="main.wasp.ts" {12}
    import { app } from "@wasp.sh/spec"

    export default app({
      name: "myApp",
      wasp: { version: "{latestWaspVersion}" },
      title: "My App",
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
  </TabItem>
</Tabs>

Read more about the `usernameAndPassword` auth method options [here](#fields-in-the-usernameandpassword-object).

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

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```ts title="main.wasp.ts"
    import { app, page, route } from "@wasp.sh/spec"
    import { LoginPage, SignupPage } from "./src/pages/auth" with { type: "ref" }

    export default app({
      // ...
      decls: [
        route("LoginRoute", "/login", page(LoginPage)),
        route("SignupRoute", "/signup", page(SignupPage)),
      ],
    })
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="main.wasp.ts"
    import { app, page, route } from "@wasp.sh/spec"
    import { LoginPage, SignupPage } from "./src/pages/auth" with { type: "ref" }

    export default app({
      // ...
      decls: [
        route("LoginRoute", "/login", page(LoginPage)),
        route("SignupRoute", "/signup", page(SignupPage)),
      ],
    })
    ```
  </TabItem>
</Tabs>

We'll define the React components for these pages in the `src/pages/auth.{jsx,tsx}` file below.

### 4. Create the Client Pages

<TailwindNote />

Let's create a `auth.{jsx,tsx}` file in the `src/pages` folder and add the following to it:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```tsx title="src/pages/auth.jsx"
    import { LoginForm, SignupForm } from "wasp/client/auth"
    import { Link } from "react-router"

    export function LoginPage() {
      return (
        <Layout>
          <LoginForm />
          <br />
          <span className="text-sm font-medium text-gray-900">
            Don't have an account yet? <Link to="/signup">go to signup</Link>.
          </span>
        </Layout>
      )
    }

    export function SignupPage() {
      return (
        <Layout>
          <SignupForm />
          <br />
          <span className="text-sm font-medium text-gray-900">
            I already have an account (<Link to="/login">go to login</Link>).
          </span>
        </Layout>
      )
    }

    // A layout component to center the content
    export function Layout({ children }) {
      return (
        <div className="h-full w-full bg-white">
          <div className="flex min-h-[75vh] min-w-full items-center justify-center">
            <div className="h-full w-full max-w-sm bg-white p-5">
              <div>{children}</div>
            </div>
          </div>
        </div>
      )
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```tsx title="src/pages/auth.tsx"
    import { LoginForm, SignupForm } from "wasp/client/auth"
    import { Link } from "react-router"

    export function LoginPage() {
      return (
        <Layout>
          <LoginForm />
          <br />
          <span className="text-sm font-medium text-gray-900">
            Don't have an account yet? <Link to="/signup">go to signup</Link>.
          </span>
        </Layout>
      )
    }

    export function SignupPage() {
      return (
        <Layout>
          <SignupForm />
          <br />
          <span className="text-sm font-medium text-gray-900">
            I already have an account (<Link to="/login">go to login</Link>).
          </span>
        </Layout>
      )
    }

    // A layout component to center the content
    export function Layout({ children }: { children: React.ReactNode }) {
      return (
        <div className="h-full w-full bg-white">
          <div className="flex min-h-[75vh] min-w-full items-center justify-center">
            <div className="h-full w-full max-w-sm bg-white p-5">
              <div>{children}</div>
            </div>
          </div>
        </div>
      )
    }
    ```
  </TabItem>
</Tabs>

We imported the generated Auth UI components and used them in our pages. Read more about the Auth UI components [here](../auth/ui).

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

### `userEntity` fields

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```ts title="main.wasp.ts"
    import { app } from "@wasp.sh/spec"

    export default app({
      name: "myApp",
      wasp: { version: "{latestWaspVersion}" },
      title: "My App",
      auth: {
        userEntity: "User",
        methods: {
          usernameAndPassword: {},
        },
        onAuthFailedRedirectTo: "/login"
      },
      // ...
    })
    ```

    ```prisma title="schema.prisma"
    model User {
      id Int @id @default(autoincrement())
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="main.wasp.ts"
    import { app } from "@wasp.sh/spec"

    export default app({
      name: "myApp",
      wasp: { version: "{latestWaspVersion}" },
      title: "My App",
      auth: {
        userEntity: "User",
        methods: {
          usernameAndPassword: {},
        },
        onAuthFailedRedirectTo: "/login"
      },
      // ...
    })
    ```

    ```prisma title="schema.prisma"
    model User {
      id Int @id @default(autoincrement())
    }
    ```
  </TabItem>
</Tabs>

<UserFieldsExplainer />

### Fields in the `usernameAndPassword` object

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```ts title="main.wasp.ts"
    import { app } from "@wasp.sh/spec"
    import { userSignupFields } from "./src/auth/email" with { type: "ref" }

    export default app({
      name: "myApp",
      wasp: { version: "{latestWaspVersion}" },
      title: "My App",
      auth: {
        userEntity: "User",
        methods: {
          usernameAndPassword: {
            userSignupFields,
          },
        },
        onAuthFailedRedirectTo: "/login"
      },
      // ...
    })
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="main.wasp.ts"
    import { app } from "@wasp.sh/spec"
    import { userSignupFields } from "./src/auth/email" with { type: "ref" }

    export default app({
      name: "myApp",
      wasp: { version: "{latestWaspVersion}" },
      title: "My App",
      auth: {
        userEntity: "User",
        methods: {
          usernameAndPassword: {
            userSignupFields,
          },
        },
        onAuthFailedRedirectTo: "/login"
      },
      // ...
    })
    ```
  </TabItem>
</Tabs>

#### `userSignupFields`: [`Reference`](../general/spec.md#reference-imports)

<UserSignupFieldsExplainer />

Read more about the `userSignupFields` function [here](./overview#1-defining-extra-fields).
