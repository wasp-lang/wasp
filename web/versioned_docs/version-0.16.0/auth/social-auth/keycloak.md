---
title: Keycloak
---

import useBaseUrl from '@docusaurus/useBaseUrl';
import DefaultBehaviour from './\_default-behaviour.md';
import OverrideIntro from './\_override-intro.md';
import OverrideExampleIntro from './\_override-example-intro.md';
import UsingAuthNote from './\_using-auth-note.md';
import WaspFileStructureNote from './\_wasp-file-structure-note.md';
import GetUserFieldsType from './\_getuserfields-type.md';
import ApiReferenceIntro from './\_api-reference-intro.md';
import UserSignupFieldsExplainer from '../\_user-signup-fields-explainer.md';
import KeycloakData from '../entities/\_keycloak-data.md';
import AccessingUserDataNote from '../\_accessing-user-data-note.md';

Wasp supports Keycloak Authentication out of the box.

[Keycloak](https://www.keycloak.org/) is an open-source identity and access management solution for modern applications and services. Keycloak provides both SAML and OpenID protocol solutions. It also has a very flexible and powerful administration UI.

Let's walk through enabling Keycloak authentication, explain some of the default settings, and show how to override them.

## Setting up Keycloak Auth

Enabling Keycloak Authentication comes down to a series of steps:

1. Enabling Keycloak authentication in the Wasp file.
1. Adding the `User` entity.
1. Creating a Keycloak client.
1. Adding the necessary Routes and Pages
1. Using Auth UI components in our Pages.

<WaspFileStructureNote />

### 1. Adding Keycloak Auth to Your Wasp File

Let's start by properly configuring the Auth object:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.15.0"
  },
  title: "My App",
  auth: {
    // 1. Specify the User entity (we'll define it next)
    // highlight-next-line
    userEntity: User,
    methods: {
      // 2. Enable Keycloak Auth
      // highlight-next-line
      keycloak: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.15.0"
  },
  title: "My App",
  auth: {
    // 1. Specify the User entity (we'll define it next)
    // highlight-next-line
    userEntity: User,
    methods: {
      // 2. Enable Keycloak Auth
      // highlight-next-line
      keycloak: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

</TabItem>
</Tabs>

The `userEntity` is explained in [the social auth overview](../../auth/social-auth/overview#social-login-entity).

### 2. Adding the User Entity

Let's now define the `app.auth.userEntity` entity in the `schema.prisma` file:

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

### 3. Creating a Keycloak Client

1. Log into your Keycloak admin console.
1. Under **Clients**, click on **Create Client**.

![Keycloak Screenshot 1](/img/auth/keycloak/1-keycloak.png)

1. Fill in the **Client ID** and choose a name for the client.

![Keycloak Screenshot 2](/img/auth/keycloak/2-keycloak.png)

1. In the next step, enable **Client Authentication**.

![Keycloak Screenshot 3](/img/auth/keycloak/3-keycloak.png)

1. Under **Valid Redirect URIs**, add `http://localhost:3001/auth/keycloak/callback` for local development.

![Keycloak Screenshot 4](/img/auth/keycloak/4-keycloak.png)

    - Once you know on which URL(s) your API server will be deployed, also add those URL(s).
    - For example: `https://my-server-url.com/auth/keycloak/callback`.

1. Click **Save**.
1. In the **Credentials** tab, copy the **Client Secret** value, which we'll use in the next step.

![Keycloak Screenshot 5](/img/auth/keycloak/5-keycloak.png)

### 4. Adding Environment Variables

Add these environment variables to the `.env.server` file at the root of your project (take their values from the previous step):

```bash title=".env.server"
KEYCLOAK_CLIENT_ID=your-keycloak-client-id
KEYCLOAK_CLIENT_SECRET=your-keycloak-client-secret
KEYCLOAK_REALM_URL=https://your-keycloak-url.com/realms/master
```

We assumed in the `KEYCLOAK_REALM_URL` env variable that you are using the `master` realm. If you are using a different realm, replace `master` with your realm name.

### 5. Adding the Necessary Routes and Pages

Let's define the necessary authentication Routes and Pages.

Add the following code to your `main.wasp` file:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...

route LoginRoute { path: "/login", to: LoginPage }
page LoginPage {
  component: import { Login } from "@src/pages/auth.jsx"
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

route LoginRoute { path: "/login", to: LoginPage }
page LoginPage {
  component: import { Login } from "@src/pages/auth.tsx"
}
```

</TabItem>
</Tabs>

We'll define the React components for these pages in the `src/pages/auth.{jsx,tsx}` file below.

### 6. Create the Client Pages

:::info
We are using [Tailwind CSS](https://tailwindcss.com/) to style the pages. Read more about how to add it [here](../../project/css-frameworks).
:::

Let's now create an `auth.{jsx,tsx}` file in the `src/pages`.
It should have the following code:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```tsx title="src/pages/auth.jsx"
import { LoginForm } from 'wasp/client/auth'

export function Login() {
  return (
    <Layout>
      <LoginForm />
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
import { LoginForm } from 'wasp/client/auth'

export function Login() {
  return (
    <Layout>
      <LoginForm />
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

:::info Auth UI
Our pages use an automatically generated Auth UI component. Read more about Auth UI components [here](../../auth/ui).
:::

### Conclusion

Yay, we've successfully set up Keycloak Auth!

Running `wasp db migrate-dev` and `wasp start` should now give you a working app with authentication.
To see how to protect specific pages (i.e., hide them from non-authenticated users), read the docs on [using auth](../../auth/overview).

## Default Behaviour

Add `keycloak: {}` to the `auth.methods` dictionary to use it with default settings:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title=main.wasp
app myApp {
  wasp: {
    version: "^0.15.0"
  },
  title: "My App",
  auth: {
    userEntity: User,
    methods: {
      // highlight-next-line
      keycloak: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title=main.wasp
app myApp {
  wasp: {
    version: "^0.15.0"
  },
  title: "My App",
  auth: {
    userEntity: User,
    methods: {
      // highlight-next-line
      keycloak: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

</TabItem>
</Tabs>

<DefaultBehaviour />

## Overrides

<OverrideIntro />

### Data Received From Keycloak

We are using Keycloak's API and its `/userinfo` endpoint to fetch the user's data.

```ts title="Keycloak user data"
{
  sub: '5adba8fc-3ea6-445a-a379-13f0bb0b6969',
  email_verified: true,
  name: 'Test User',
  preferred_username: 'test',
  given_name: 'Test',
  family_name: 'User',
  email: 'test@example.com'
}
```

The fields you receive will depend on the scopes you requested. The default scope is set to `profile` only. If you want to get the user's email, you need to specify the `email` scope in the `configFn` function.

<small>

For up-to-date info about the data received from Keycloak, please refer to the [Keycloak API documentation](https://www.keycloak.org/docs-api/23.0.7/javadocs/org/keycloak/representations/UserInfo.html).
</small>

### Using the Data Received From Keycloak

<OverrideExampleIntro />

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.15.0"
  },
  title: "My App",
  auth: {
    userEntity: User,
    methods: {
      keycloak: {
        // highlight-next-line
        configFn: import { getConfig } from "@src/auth/keycloak.js",
        // highlight-next-line
        userSignupFields: import { userSignupFields } from "@src/auth/keycloak.js"
      }
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

```prisma title="schema.prisma"
model User {
  id          Int    @id @default(autoincrement())
  username    String @unique
  displayName String
}

// ...
```

```js title=src/auth/keycloak.js
export const userSignupFields = {
  username: () => 'hardcoded-username',
  displayName: (data) => data.profile.name,
}

export function getConfig() {
  return {
    scopes: ['profile', 'email'],
  }
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.15.0"
  },
  title: "My App",
  auth: {
    userEntity: User,
    methods: {
      keycloak: {
        // highlight-next-line
        configFn: import { getConfig } from "@src/auth/keycloak.js",
        // highlight-next-line
        userSignupFields: import { userSignupFields } from "@src/auth/keycloak.js"
      }
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

```prisma title="schema.prisma"
model User {
  id          Int    @id @default(autoincrement())
  username    String @unique
  displayName String
}

// ...
```

```ts title=src/auth/keycloak.ts
import { defineUserSignupFields } from 'wasp/server/auth'

export const userSignupFields = defineUserSignupFields({
  username: () => 'hardcoded-username',
  displayName: (data: any) => data.profile.name,
})

export function getConfig() {
  return {
    scopes: ['profile', 'email'],
  }
}
```

<GetUserFieldsType />

</TabItem>
</Tabs>

## Using Auth

<UsingAuthNote />

When you receive the `user` object [on the client or the server](../overview.md#accessing-the-logged-in-user), you'll be able to access the user's Keycloak ID like this:

<KeycloakData />

<AccessingUserDataNote />

## API Reference

<ApiReferenceIntro />

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.15.0"
  },
  title: "My App",
  auth: {
    userEntity: User,
    methods: {
      keycloak: {
        // highlight-next-line
        configFn: import { getConfig } from "@src/auth/keycloak.js",
        // highlight-next-line
        userSignupFields: import { userSignupFields } from "@src/auth/keycloak.js"
      }
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.15.0"
  },
  title: "My App",
  auth: {
    userEntity: User,
    methods: {
      keycloak: {
        // highlight-next-line
        configFn: import { getConfig } from "@src/auth/keycloak.js",
        // highlight-next-line
        userSignupFields: import { userSignupFields } from "@src/auth/keycloak.js"
      }
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

</TabItem>
</Tabs>

The `keycloak` dict has the following properties:

- #### `configFn: ExtImport`

  This function must return an object with the scopes for the OAuth provider.

  <Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">

  ```js title=src/auth/keycloak.js
  export function getConfig() {
    return {
      scopes: ['profile', 'email'],
    }
  }
  ```

  </TabItem>
  <TabItem value="ts" label="TypeScript">

  ```ts title=src/auth/keycloak.ts
  export function getConfig() {
    return {
      scopes: ['profile', 'email'],
    }
  }
  ```

  </TabItem>
  </Tabs>

- #### `userSignupFields: ExtImport`

  <UserSignupFieldsExplainer />

  Read more about the `userSignupFields` function [here](../overview#1-defining-extra-fields).
