---
title: Google
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
import GoogleData from '../entities/\_google-data.md';
import AccessingUserDataNote from '../\_accessing-user-data-note.md';

Wasp supports Google Authentication out of the box.
Google Auth is arguably the best external auth option, as most users on the web already have Google accounts.

Enabling it lets your users log in using their existing Google accounts, greatly simplifying the process and enhancing the user experience.

Let's walk through enabling Google authentication, explain some of the default settings, and show how to override them.

## Setting up Google Auth

Enabling Google Authentication comes down to a series of steps:

1. Enabling Google authentication in the Wasp file.
1. Adding the `User` entity.
1. Creating a Google OAuth app.
1. Adding the necessary Routes and Pages
1. Using Auth UI components in our Pages.

<WaspFileStructureNote />

### 1. Adding Google Auth to Your Wasp File

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
      // 2. Enable Google Auth
      // highlight-next-line
      google: {}
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
      // 2. Enable Google Auth
      // highlight-next-line
      google: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

</TabItem>
</Tabs>

`userEntity` is explained in [the social auth overview](../../auth/social-auth/overview#social-login-entity).

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

### 3. Creating a Google OAuth App

To use Google as an authentication method, you'll first need to create a Google project and provide Wasp with your client key and secret. Here's how you do it:

1. Create a Google Cloud Platform account if you do not already have one: https://cloud.google.com/
2. Create and configure a new Google project here: https://console.cloud.google.com/home/dashboard

![Google Console Screenshot 1](/img/integrations-google-1.jpg)

![Google Console Screenshot 2](/img/integrations-google-2.jpg)

3. Search for **OAuth** in the top bar, click on **OAuth consent screen**.

![Google Console Screenshot 3](/img/integrations-google-3.jpg)

- Select what type of app you want, we will go with **External**.

  ![Google Console Screenshot 4](/img/integrations-google-4.jpg)

- Fill out applicable information on Page 1.

  ![Google Console Screenshot 5](/img/integrations-google-5.jpg)

- On Page 2, Scopes, you should select `userinfo.profile`. You can optionally search for other things, like `email`.

  ![Google Console Screenshot 6](/img/integrations-google-6.jpg)

  ![Google Console Screenshot 7](/img/integrations-google-7.jpg)

  ![Google Console Screenshot 8](/img/integrations-google-8.jpg)

- Add any test users you want on Page 3.

  ![Google Console Screenshot 9](/img/integrations-google-9.jpg)

4. Next, click **Credentials**.

![Google Console Screenshot 10](/img/integrations-google-10.jpg)

- Select **Create Credentials**.
- Select **OAuth client ID**.

  ![Google Console Screenshot 11](/img/integrations-google-11.jpg)

- Complete the form

  ![Google Console Screenshot 12](/img/integrations-google-12.jpg)

- Under Authorized redirect URIs, put in: `http://localhost:3001/auth/google/callback`

  ![Google Console Screenshot 13](/img/integrations-google-13.jpg)

  - Once you know on which URL(s) your API server will be deployed, also add those URL(s).
    - For example: `https://your-server-url.com/auth/google/callback`

- When you save, you can click the Edit icon and your credentials will be shown.

  ![Google Console Screenshot 14](/img/integrations-google-14.jpg)

5. Copy your Client ID and Client secret as you will need them in the next step.

### 4. Adding Environment Variables

Add these environment variables to the `.env.server` file at the root of your project (take their values from the previous step):

```bash title=".env.server"
GOOGLE_CLIENT_ID=your-google-client-id
GOOGLE_CLIENT_SECRET=your-google-client-secret
```

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

Let's now create a `auth.{jsx,tsx}` file in the `src/pages`.
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
Our pages use an automatically-generated Auth UI component. Read more about Auth UI components [here](../../auth/ui).
:::

### Conclusion

Yay, we've successfully set up Google Auth! 🎉

![Google Auth](/img/auth/google.png)

Running `wasp db migrate-dev` and `wasp start` should now give you a working app with authentication.
To see how to protect specific pages (i.e., hide them from non-authenticated users), read the docs on [using auth](../../auth/overview).

## Default Behaviour

Add `google: {}` to the `auth.methods` dictionary to use it with default settings:

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
      google: {}
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
      google: {}
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

### Data Received From Google

We are using Google's API and its `/userinfo` endpoint to fetch the user's data.

The data received from Google is an object which can contain the following fields:

```json
[
  "name",
  "given_name",
  "family_name",
  "email",
  "email_verified",
  "aud",
  "exp",
  "iat",
  "iss",
  "locale",
  "picture",
  "sub"
]
```

The fields you receive depend on the scopes you request. The default scope is set to `profile` only. If you want to get the user's email, you need to specify the `email` scope in the `configFn` function.

<small>

For an up to date info about the data received from Google, please refer to the [Google API documentation](https://developers.google.com/identity/openid-connect/openid-connect#an-id-tokens-payload).
</small>

### Using the Data Received From Google

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
      google: {
        // highlight-next-line
        configFn: import { getConfig } from "@src/auth/google.js",
        // highlight-next-line
        userSignupFields: import { userSignupFields } from "@src/auth/google.js"
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

```js title=src/auth/google.js
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
      google: {
        // highlight-next-line
        configFn: import { getConfig } from "@src/auth/google.js",
        // highlight-next-line
        userSignupFields: import { userSignupFields } from "@src/auth/google.js"
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

```ts title=src/auth/google.ts
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

When you receive the `user` object [on the client or the server](../overview.md#accessing-the-logged-in-user), you'll be able to access the user's Google ID like this:

<GoogleData />

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
      google: {
        // highlight-next-line
        configFn: import { getConfig } from "@src/auth/google.js",
        // highlight-next-line
        userSignupFields: import { userSignupFields } from "@src/auth/google.js"
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
      google: {
        // highlight-next-line
        configFn: import { getConfig } from "@src/auth/google.js",
        // highlight-next-line
        userSignupFields: import { userSignupFields } from "@src/auth/google.js"
      }
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

</TabItem>
</Tabs>

The `google` dict has the following properties:

- #### `configFn: ExtImport`

  This function must return an object with the scopes for the OAuth provider.

  <Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">

  ```js title=src/auth/google.js
  export function getConfig() {
    return {
      scopes: ['profile', 'email'],
    }
  }
  ```

  </TabItem>
  <TabItem value="ts" label="TypeScript">

  ```ts title=src/auth/google.ts
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
