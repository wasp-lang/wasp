---
title: GitHub
---

import useBaseUrl from '@docusaurus/useBaseUrl';

TODO: write an introduction

## Setting up Github Auth

We'll need to take the following steps to set up Github authentication:
1. Enable Github authentication in the Wasp file
1. Add the entities
1. Create a Github OAuth app
1. Add the routes and pages
1. Use Auth UI components in our pages

Structure of the `main.wasp` file we will end up with:

```wasp title="main.wasp"
// Configuring e-mail authentication
app myApp {
  auth: { ... }
}

// Defining User entity
entity User { ... }
entity SocialLogin { ... }

// Defining routes and pages
route LoginRoute { ... }
page LoginPage { ... }
```

### 1. Add Github Auth to Your Wasp File

To implement Github Auth, you'll need to add the Auth object with the following configuration to your `main.wasp` file:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.11.0"
  },
  title: "My App",
  auth: {
    // 1. Specify the User entity
    userEntity: User,
    // 2. Specify the SocialLogin entity
    externalAuthEntity: SocialLogin,
    methods: {
      // 3. Enable Github Auth
      gitHub: {}
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
    version: "^0.11.0"
  },
  title: "My App",
  auth: {
    // 1. Specify the User entity
    userEntity: User,
    // 2. Specify the SocialLogin entity
    externalAuthEntity: SocialLogin,
    methods: {
      // 3. Enable Github Auth
      gitHub: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```
</TabItem>
</Tabs>

TODO: link to `externalAuthEntity` in the Overview docs

### 2. Add the Entities

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...
// 4. Define the User entity
entity User {=psl
    id          Int     @id @default(autoincrement())
    // ...
    externalAuthAssociations  SocialLogin[]
psl=}

// 5. Define the SocialLogin entity
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


```wasp title="main.wasp"
// ...
// 4. Define the User entity
entity User {=psl
    id          Int     @id @default(autoincrement())
    // ...
    externalAuthAssociations  SocialLogin[]
psl=}

// 5. Define the SocialLogin entity
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


### 3. Create a GitHub OAuth App

To use GitHub as an authentication method, you'll first need to create a GitHub OAuth App and provide Wasp with your client key and secret.

Here is how to do so:

1. Log into your GitHub account and navigate to: https://github.com/settings/developers
2. Select **New OAuth App**
3. Supply required information

  <img alt="GitHub Applications Screenshot"
      src={useBaseUrl('img/integrations-github-1.png')}
      width="400px"
  />

  - For **Authorization callback URL**:
    - while in development, put: `http://localhost:3000/auth/login/github`
    - once you know on which URL your app will be deployed, you can create a new app with that URL instead e.g. `https://someotherhost.com/auth/login/github`

4. Hit **Register application**
5. Copy your Client ID and Client secret as you'll need them in the next step.

### 4. Add Environment Variables

Lastly, you'll need to add these environment variables to your `.env.server` file at the root of your project:

```bash title=".env.server"
GITHUB_CLIENT_ID=your-github-client-id
GITHUB_CLIENT_SECRET=your-github-client-secret
```

### 5. Add the Routes and Pages

Next, we need to define the routes and pages for the authentication pages.

Add the following to the `main.wasp` file:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...

// 6. Define the routes
route LoginRoute { path: "/login", to: LoginPage }
page LoginPage {
  component: import { Login } from "@client/pages/auth.jsx"
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

// 6. Define the routes
route LoginRoute { path: "/login", to: LoginPage }
page LoginPage {
  component: import { Login } from "@client/pages/auth.tsx"
}
```
</TabItem>
</Tabs>

We'll define the React components for these pages in the `client/pages/auth.{jsx,tsx}` file below.

### 6. Create the Client Pages

:::info
We are using [Tailwind CSS](https://tailwindcss.com/) to style the pages. Read more about how to add it [here](/docs/project/css-frameworks).
:::

Let's create a `auth.{jsx,tsx}` file in the `client/pages` folder and add the following to it:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```tsx title="client/pages/auth.jsx"
import { LoginForm } from "@wasp/auth/forms/Login";

export function Login() {
  return (
    <Layout>
      <LoginForm />
    </Layout>
  );
}

// A layout component to center the content
export function Layout({ children }) {
  return (
    <div className="w-full h-full bg-white">
      <div className="min-w-full min-h-[75vh] flex items-center justify-center">
        <div className="w-full h-full max-w-sm p-5 bg-white">
          <div>{children}</div>
        </div>
      </div>
    </div>
  );
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="client/pages/auth.tsx"
import { LoginForm } from "@wasp/auth/forms/Login";

export function Login() {
  return (
    <Layout>
      <LoginForm />
    </Layout>
  );
}

// A layout component to center the content
export function Layout({ children }: { children: React.ReactNode }) {
  return (
    <div className="w-full h-full bg-white">
      <div className="min-w-full min-h-[75vh] flex items-center justify-center">
        <div className="w-full h-full max-w-sm p-5 bg-white">
          <div>{children}</div>
        </div>
      </div>
    </div>
  );
}
```
</TabItem>
</Tabs>

We imported the generated Auth UI component and used them in our pages. Read more about the Auth UI components [here](/docs/auth/ui).

### Conclusion

Yay, now you set up Github Auth! 🎉 Running `wasp db migrate-dev` and then `wasp start` should give you a working app with authentication. If you want to put some of the pages behind authentication, read the [using auth docs](/docs/auth/overview).

TOOD: put a screenshot

## Default Settings

```wasp title=main.wasp {10}
app myApp {
  wasp: {
    version: "^0.11.0"
  },
  title: "My App",
  auth: {
    userEntity: User,
    externalAuthEntity: SocialLogin,
    methods: {
      gitHub: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

Add `gitHub: {}` to your `auth.methods` dictionary to use it with default settings.

By default, Wasp expects you to set two environment variables to use GitHub authentication:

- `GITHUB_CLIENT_ID`
- `GITHUB_CLIENT_SECRET`

Read [above](#2-create-a-github-oauth-app) for how to get these values.


## Overrides

When a user signs in for the first time, Wasp will create a new User account and link it to the chosen Auth Provider account for future logins. If the `userEntity` contains a `username` field it will default to a random dictionary phrase that does not exist in the database, such as `nice-blue-horse-27160`. This is a historical coupling between auth methods that will be removed over time.

If you would like to allow the user to select their username, or some other sign up flow, you could add a boolean property to your `User` entity indicating the account setup is incomplete. You can then check this user's property on the client with the [`useAuth()`](#useauth) hook and redirect them when appropriate
  - e.g. check on homepage if `user.isAuthSetup === false`, redirect them to `EditUserDetailsPage` where they can edit the `username` property.

Alternatively, you could add a `displayName` property to your User entity and assign it using the details of their provider account. Below is an example of how to do this by using:
  - the `getUserFieldsFn` function to configure the user's `username` or `displayName` from their provider account

We also show you how to customize the configuration of the Provider's settings using:
  - the `configFn` function

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

//...

```

## Using Auth

To read more about how to set up the logout button and how to get access to the logged-in user in our client and server code, read the [using auth docs](/docs/auth/overview).

## Options Reference

TODO: make sure all the example show github examples

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.11.0"
  },
  title: "My App",
  auth: {
    userEntity: User,
    externalAuthEntity: SocialLogin,
    methods: {
      gitHub: {
        configFn: import { config } from "@server/auth/github.js",
        getUserFieldsFn: import { getUserFields } from "@server/auth/github.js"
      }
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

The `github` dict has the following properties:

- #### `configFn: ServerImport`

    This function should return an object with the following shape:

    ```js title=src/server/auth/github.js
    export function config() {
      // ...
      return {
        clientID, // look up from env or elsewhere,
        clientSecret, // look up from env or elsewhere,
        scope: [] // default is an empty array for GitHub
      }
    }

    // ...
    ```

    <p>Here is a link to the <a href="https://github.com/wasp-lang/wasp/blob/release/waspc/data/Generator/templates/server/src/routes/auth/passport/github/defaults.js">default implementations</a> as a reference</p>

- #### `getUserFieldsFn: ServerImport`

  This function should return the user fields to use when creating a new user upon their first time logging in with a Social Auth Provider. The context contains a User entity for DB access, and the args are what the OAuth provider responds with. Here is how you could generate a username based on the Github display name. In your model, you could choose to add more attributes and set additional information.
    ```js title=src/server/auth/github.js
    import { generateAvailableUsername } from '@wasp/core/auth.js'

    // ...

    export async function getUserFields(_context, args) {
      const username = await generateAvailableUsername(args.profile.displayName.split(' '), { separator: '.' })
      return { username }
    }
    ```

    Or you could set the optional `displayName` property on the `User` entity instead:
    ```js title=src/server/auth/github.js
    import { generateAvailableDictionaryUsername, generateAvailableUsername } from '@wasp/core/auth.js'

    // ...

    export async function getUserFields(_context, args) {
      const username = await generateAvailableDictionaryUsername()
      const displayName = await generateAvailableUsername(args.profile.displayName.split(' '), { separator: '.' })
      return { username, displayName }
    }
    ```
    - `generateAvailableUsername` takes an array of Strings and an optional separator and generates a string ending with a random number that is not yet in the database. For example, the above could produce something like "Jim.Smith.3984" for a Github user Jim Smith.
    - `generateAvailableDictionaryUsername` generates a random dictionary phrase that is not yet in the database. For example, `nice-blue-horse-27160`.