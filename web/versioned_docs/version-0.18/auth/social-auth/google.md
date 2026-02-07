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
2. Adding the `User` entity.
3. Creating a Google OAuth app.
4. Adding the necessary Routes and Pages
5. Using Auth UI components in our Pages.

<WaspFileStructureNote />

### 1. Adding Google Auth to Your Wasp File

Let's start by properly configuring the Auth object:

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "{latestWaspVersion}"
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

`userEntity` is explained in [the social auth overview](./overview.md#user-entity).

### 2. Adding the User Entity

Let's now define the `app.auth.userEntity` entity in the `schema.prisma` file:

```prisma title="schema.prisma"
// 3. Define the user entity
model User {
  // highlight-next-line
  id Int @id @default(autoincrement())
  // Add your own fields below
  // ...
}
```

### 3. Creating a Google OAuth App

To use Google as an authentication method, you'll first need to create a Google project and provide Wasp with your client key and secret. Here's how you do it:

1. Create a Google Cloud Platform account if you do not already have one: https://cloud.google.com/
2. Create and configure a new Google project here: https://console.cloud.google.com/projectcreate

    ![Google Console Screenshot 1](/img/integrations-google-v2-1.png)

    ![Google Console Screenshot 2](/img/integrations-google-v2-2.png)

3. Search for **Google Auth** in the top bar (1), click on **Google Auth Platform** (2). Then click on **Get Started** (3).

    ![Google Console Screenshot 3](/img/integrations-google-v2-3.png)

    ![Google Console Screenshot 4](/img/integrations-google-v2-4.png)

4. Fill out you app information. For the **Audience** field, we will go with **External**. When you're done, click **Create**.

    ![Google Console Screenshot 5](/img/integrations-google-v2-5.png)

    ![Google Console Screenshot 6](/img/integrations-google-v2-6.png)

5. You should now be in the **OAuth Overview** page. Click on **Create OAuth Client** (1).

    ![Google Console Screenshot 7](/img/integrations-google-v2-7.png)

6. Fill out the form. These are the values for a typical Wasp application:

    | # | Field                    | Value                                        |
    | - | ------------------------ | -------------------------------------------- |
    | 1 | Application type         | Web application                              |
    | 2 | Name                     | (your wasp app name)                         |
    | 3 | Authorized redirect URIs | `http://localhost:3001/auth/google/callback` |

    :::note
    Once you know on which URL(s) your API server will be deployed, also add those URL(s) to the **Authorized redirect URIs**.\
    For example: `https://your-server-url.com/auth/google/callback`
    :::

    ![Google Console Screenshot 8](/img/integrations-google-v2-8.png)

    Then click on **Create** (4).

7. You will see a box saying **OAuth client created**. Click on **OK**.

    ![Google Console Screenshot 9](/img/integrations-google-v2-9.png)

8. Click on the name of your newly-created app.

    ![Google Console Screenshot 10](/img/integrations-google-v2-10.png)

9. On the right-hand side, you will see your **Client ID** (1) and **Client secret** (2). **Copy them somewhere safe, as you will need them for your app.**

    ![Google Console Screenshot 11](/img/integrations-google-v2-11.png)

    :::info
    These are the credentials your app will use to authenticate with Google. Do not share them anywhere publicly, as anyone with these credentials can impersonate your app and access user data.
    :::

10. Click on **Data Access** (1) in the left-hand menu, then click on **Add or remove scopes** (2). You should select `userinfo.profile` (3), and optionally `userinfo.email` (4), or any other scopes you want to use. Remember to click **Update** and **Save** when done.

    ![Google Console Screenshot 12](/img/integrations-google-v2-12.png)

    ![Google Console Screenshot 13](/img/integrations-google-v2-13.png)

11. Go to **Audience** (1) in the left-hand menu, and add any test users you want (2). This is useful for testing your app before going live. You can add any email addresses you want to test with.

    ![Google Console Screenshot 14](/img/integrations-google-v2-14.png)

12. Finally, you can go to **Branding** (1) in the left-hand menu, and customize your app's branding in the Google login page. _This is optional_, but recommended if you want to make your app look more professional.

    ![Google Console Screenshot 15](/img/integrations-google-v2-15.png)

### 4. Adding Environment Variables

Add these environment variables to the `.env.server` file at the root of your project (take their values from the previous step):

```bash title=".env.server"
GOOGLE_CLIENT_ID=your-google-client-id
GOOGLE_CLIENT_SECRET=your-google-client-secret
```

### 5. Adding the Necessary Routes and Pages

Let's define the necessary authentication Routes and Pages.

Add the following code to your `main.wasp` file:

```wasp title="main.wasp"
// ...

route LoginRoute { path: "/login", to: LoginPage }
page LoginPage {
  component: import { Login } from "@src/pages/auth"
}
```

We'll define the React components for these pages in the `src/pages/auth.{jsx,tsx}` file below.

### 6. Create the Client Pages

:::info
We are using [Tailwind CSS](https://tailwindcss.com/) to style the pages. Read more about how to add it [here](../../project/css-frameworks).
:::

Let's now create a `auth.{jsx,tsx}` file in the `src/pages`.
It should have the following code:

```tsx title="src/pages/auth.tsx" auto-js
import type { ReactNode } from 'react'
import { LoginForm } from 'wasp/client/auth'

export function Login() {
  return (
    <Layout>
      <LoginForm />
    </Layout>
  )
}

// A layout component to center the content
export function Layout({ children }: { children: ReactNode }) {
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

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "{latestWaspVersion}"
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
        configFn: import { getConfig } from "@src/auth/google",
        // highlight-next-line
        userSignupFields: import { userSignupFields } from "@src/auth/google"
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

```ts title="src/auth/google.ts" auto-js
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

## Using Auth

<UsingAuthNote />

When you receive the `user` object [on the client or the server](../overview.md#accessing-the-logged-in-user), you'll be able to access the user's Google ID like this:

<GoogleData />

<AccessingUserDataNote />

## API Reference

<ApiReferenceIntro />

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
        configFn: import { getConfig } from "@src/auth/google",
        // highlight-next-line
        userSignupFields: import { userSignupFields } from "@src/auth/google"
      }
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

The `google` dict has the following properties:

- #### `configFn: ExtImport`

  This function must return an object with the scopes for the OAuth provider.

  ```ts title="src/auth/google.ts" auto-js
  export function getConfig() {
    return {
      scopes: ['profile', 'email'],
    }
  }
  ```

- #### `userSignupFields: ExtImport`

  <UserSignupFieldsExplainer />

  Read more about the `userSignupFields` function [here](../overview#1-defining-extra-fields).
