---
title: GitHub
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
import GithubData from '../entities/\_github-data.md';
import AccessingUserDataNote from '../\_accessing-user-data-note.md';

Wasp supports GitHub Authentication out of the box.
GitHub is a great external auth choice when you're building apps for developers, as most of them already have a GitHub account.

Letting your users log in using their GitHub accounts turns the signup process into a breeze.

Let's walk through enabling GitHub Authentication, explain some of the default settings, and show how to override them.

## Setting up GitHub Auth

Enabling GitHub Authentication comes down to a series of steps:

1. Enabling GitHub authentication in the Wasp file.
2. Adding the `User` entity.
3. Creating a GitHub OAuth app.
4. Adding the necessary Routes and Pages
5. Using Auth UI components in our Pages.

<WaspFileStructureNote />

### 1. Adding GitHub Auth to Your Wasp File

Let's start by properly configuring the Auth object:

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "{latestWaspVersion}"
  },
  title: "My App",
  auth: {
    // highlight-next-line
    // 1. Specify the User entity  (we'll define it next)
    // highlight-next-line
    userEntity: User,
    methods: {
      // highlight-next-line
      // 2. Enable GitHub Auth
      // highlight-next-line
      gitHub: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

### 2. Add the User Entity

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

### 3. Creating a GitHub OAuth App

To use GitHub as an authentication method, you'll first need to create a GitHub OAuth App and provide Wasp with your client key and secret. Here's how you do it:

1. Log into your GitHub account and navigate to: https://github.com/settings/developers.
2. Select **New OAuth App**.
3. Supply required information.

<img alt="GitHub Applications Screenshot" src={useBaseUrl('img/integrations-github-1.png')} width="400px" />

- For **Authorization callback URL**:
  - For development, put: `http://localhost:3001/auth/github/callback`.
  - Once you know on which URL your API server will be deployed, you can create a new app with that URL instead e.g. `https://your-server-url.com/auth/github/callback`.

4. Hit **Register application**.
5. Hit **Generate a new client secret** on the next page.
6. Copy your Client ID and Client secret as you'll need them in the next step.

### 4. Adding Environment Variables

Add these environment variables to the `.env.server` file at the root of your project (take their values from the previous step):

```bash title=".env.server"
GITHUB_CLIENT_ID=your-github-client-id
GITHUB_CLIENT_SECRET=your-github-client-secret
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

### 6. Creating the Client Pages

:::info
We are using [Tailwind CSS](https://tailwindcss.com/) to style the pages. Read more about how to add it [here](../../project/css-frameworks).
:::

Let's create a `auth.{jsx,tsx}` file in the `src/pages` folder and add the following to it:

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

We imported the generated Auth UI components and used them in our pages. Read more about the Auth UI components [here](../../auth/ui).

### Conclusion

Yay, we've successfully set up GitHub Auth! 🎉

![GitHub Auth](/img/auth/github.png)

Running `wasp db migrate-dev` and `wasp start` should now give you a working app with authentication.
To see how to protect specific pages (i.e., hide them from non-authenticated users), read the docs on [using auth](../../auth/overview).

## Default Behaviour

Add `gitHub: {}` to the `auth.methods` dictionary to use it with default settings.

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
      gitHub: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

<DefaultBehaviour />

## Overrides

<OverrideIntro />

### Data Received From GitHub

We are using GitHub's API and its `/user` and `/user/emails` endpoints to get the user data.

:::info We combine the data from the two endpoints

You'll find the emails in the `emails` property in the object that you receive in `userSignupFields`.

This is because we combine the data from the `/user` and `/user/emails` endpoints **if the `user` or `user:email` scope is requested.**

:::

The data we receive from GitHub on the `/user` endpoint looks something this:

```json
{
  "login": "octocat",
  "id": 1,
  "name": "monalisa octocat",
  "avatar_url": "https://github.com/images/error/octocat_happy.gif",
  "gravatar_id": ""
  // ...
}
```

And the data from the `/user/emails` endpoint looks something like this:

```json
[
  {
    "email": "octocat@github.com",
    "verified": true,
    "primary": true,
    "visibility": "public"
  }
]
```

The fields you receive will depend on the scopes you requested. By default we don't specify any scopes. If you want to get the emails, you need to specify the `user` or `user:email` scope in the `configFn` function.

<small>
  For an up to date info about the data received from GitHub, please refer to the [GitHub API documentation](https://docs.github.com/en/rest/users/users?apiVersion=2022-11-28#get-the-authenticated-user).
</small>

### Using the Data Received From GitHub

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
      gitHub: {
        // highlight-next-line
        configFn: import { getConfig } from "@src/auth/github",
        // highlight-next-line
        userSignupFields: import { userSignupFields } from "@src/auth/github"
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

```ts title="src/auth/github.ts" auto-js
import { defineUserSignupFields } from 'wasp/server/auth'

export const userSignupFields = defineUserSignupFields({
  username: () => 'hardcoded-username',
  displayName: (data: any) => data.profile.name,
})

export function getConfig() {
  return {
    scopes: ['user'],
  }
}
```

<GetUserFieldsType />

## Using Auth

<UsingAuthNote />

When you receive the `user` object [on the client or the server](../overview.md#accessing-the-logged-in-user), you'll be able to access the user's GitHub ID like this:

<GithubData />

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
      gitHub: {
        // highlight-next-line
        configFn: import { getConfig } from "@src/auth/github",
        // highlight-next-line
        userSignupFields: import { userSignupFields } from "@src/auth/github"
      }
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

The `gitHub` dict has the following properties:

- #### `configFn: ExtImport`

  This function should return an object with the scopes for the OAuth provider.

  ```ts title="src/auth/github.ts" auto-js
  export function getConfig() {
    return {
      scopes: [],
    }
  }
  ```

- #### `userSignupFields: ExtImport`

  <UserSignupFieldsExplainer />

  Read more about the `userSignupFields` function [here](../overview#1-defining-extra-fields).
