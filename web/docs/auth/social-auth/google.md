---
title: Google
---

import useBaseUrl from '@docusaurus/useBaseUrl';
import DefaultBehaviour from './\_default-behaviour.md';
import OverrideIntro from './\_override-intro.md';
import OverrideExampleIntro from './\_override-example-intro.md';
import UsingAuthNote from './\_using-auth-note.md';
import WaspFileStructureNote from './\_wasp-file-structure-note.md';
import UsernameGenerateExplanation from './\_username-generate-explanation.md';
import GetUserFieldsType from './\_getuserfields-type.md';
import ApiReferenceIntro from './\_api-reference-intro.md';

Wasp supports Google Authentication out of the box.
Google Auth is arguably the best external auth option, as most users on the web already have Google accounts.

Enabling it lets your users log in using their existing Google accounts, greatly simplifying the process and enhancing the user experience.

Let's walk through enabling Google authentication, explain some of the default settings, and show how to override them.

## Setting up Google Auth

Enabling Google Authentication comes down to a series of steps:

1. Enabling Google authentication in the Wasp file.
1. Adding the necessary Entities.
1. Creating a Google OAuth app.
1. Adding the neccessary Routes and Pages
1. Using Auth UI components in our Pages.

<WaspFileStructureNote />

### 1. Adding Google Auth to Your Wasp File

Let's start by properly configuring the Auth object:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.11.0"
  },
  title: "My App",
  auth: {
    // highlight-next-line
    // 1. Specify the User entity (we'll define it next)
    // highlight-next-line
    userEntity: User,
    // highlight-next-line
    // 2. Specify the SocialLogin entity (we'll define it next)
    // highlight-next-line
    externalAuthEntity: SocialLogin,
    methods: {
      // highlight-next-line
      // 3. Enable Google Auth
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
    version: "^0.11.0"
  },
  title: "My App",
  auth: {
    // highlight-next-line
    // 1. Specify the User entity (we'll define it next)
    // highlight-next-line
    userEntity: User,
    // highlight-next-line
    // 2. Specify the SocialLogin entity (we'll define it next)
    // highlight-next-line
    externalAuthEntity: SocialLogin,
    methods: {
      // highlight-next-line
      // 3. Enable Google Auth
      // highlight-next-line
      google: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

</TabItem>
</Tabs>

`externalAuthEntity` and `userEntity` are explained in [the social auth overview](../../auth/social-auth/overview#social-login-entity).

### 2. Adding the Entities

Let's now define the entities acting as `app.auth.userEntity` and `app.auth.externalAuthEntity`:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...
// highlight-next-line
// 4. Define the User entity
// highlight-next-line
entity User {=psl
    id          Int     @id @default(autoincrement())
    // ...
    externalAuthAssociations  SocialLogin[]
psl=}

// highlight-next-line
// 5. Define the SocialLogin entity
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

```wasp title="main.wasp"
// ...
// highlight-next-line
// 4. Define the User entity
// highlight-next-line
entity User {=psl
    id          Int     @id @default(autoincrement())
    // ...
    externalAuthAssociations  SocialLogin[]
psl=}

// highlight-next-line
// 5. Define the SocialLogin entity
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

- Under Authorized redirect URIs, put in: `http://localhost:3000/auth/login/google`

  ![Google Console Screenshot 13](/img/integrations-google-13.jpg)

  - Once you know on which URL(s) your API server will be deployed, also add those URL(s).
    - For example: `https://someotherhost.com/auth/login/google`

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
We are using [Tailwind CSS](https://tailwindcss.com/) to style the pages. Read more about how to add it [here](../../project/css-frameworks).
:::

Let's now create a `auth.{jsx,tsx}` file in the `client/pages`.
It should have the following code:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```tsx title="client/pages/auth.jsx"
import { LoginForm } from '@wasp/auth/forms/Login'

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
    <div className="w-full h-full bg-white">
      <div className="min-w-full min-h-[75vh] flex items-center justify-center">
        <div className="w-full h-full max-w-sm p-5 bg-white">
          <div>{children}</div>
        </div>
      </div>
    </div>
  )
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="client/pages/auth.tsx"
import { LoginForm } from '@wasp/auth/forms/Login'

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
    <div className="w-full h-full bg-white">
      <div className="min-w-full min-h-[75vh] flex items-center justify-center">
        <div className="w-full h-full max-w-sm p-5 bg-white">
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
      google: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

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

### Using the User's Provider Account Details

<OverrideExampleIntro />

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp" {11-12,22}
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
        configFn: import { getConfig } from "@server/auth/google.js",
        getUserFieldsFn: import { getUserFields } from "@server/auth/google.js"
      }
    },
    onAuthFailedRedirectTo: "/login"
  },
}

entity User {=psl
    id                        Int     @id @default(autoincrement())
    username                  String  @unique
    displayName               String
    externalAuthAssociations  SocialLogin[]
psl=}

// ...
```

```js title=src/server/auth/google.js
import { generateAvailableDictionaryUsername } from '@wasp/core/auth.js'

export const getUserFields = async (_context, args) => {
  const username = await generateAvailableDictionaryUsername()
  const displayName = args.profile.displayName
  return { username, displayName }
}

export function getConfig() {
  return {
    clientID, // look up from env or elsewhere
    clientSecret, // look up from env or elsewhere
    scope: ['profile', 'email'],
  }
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp" {11-12,22}
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
        configFn: import { getConfig } from "@server/auth/google.js",
        getUserFieldsFn: import { getUserFields } from "@server/auth/google.js"
      }
    },
    onAuthFailedRedirectTo: "/login"
  },
}

entity User {=psl
    id                        Int     @id @default(autoincrement())
    username                  String  @unique
    displayName               String
    externalAuthAssociations  SocialLogin[]
psl=}

// ...
```

```ts title=src/server/auth/google.ts
import type { GetUserFieldsFn } from '@wasp/types'
import { generateAvailableDictionaryUsername } from '@wasp/core/auth.js'

export const getUserFields: GetUserFieldsFn = async (_context, args) => {
  const username = await generateAvailableDictionaryUsername()
  const displayName = args.profile.displayName
  return { username, displayName }
}

export function getConfig() {
  return {
    clientID, // look up from env or elsewhere
    clientSecret, // look up from env or elsewhere
    scope: ['profile', 'email'],
  }
}
```

<GetUserFieldsType />

</TabItem>
</Tabs>

## Using Auth

<UsingAuthNote />

## API Reference

<ApiReferenceIntro />

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp" {11-12}
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
        configFn: import { getConfig } from "@server/auth/google.js",
        getUserFieldsFn: import { getUserFields } from "@server/auth/google.js"
      }
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp" {11-12}
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
        configFn: import { getConfig } from "@server/auth/google.js",
        getUserFieldsFn: import { getUserFields } from "@server/auth/google.js"
      }
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

</TabItem>
</Tabs>

The `google` dict has the following properties:

- #### `configFn: ServerImport`

  This function must return an object with the Client ID, the Client Secret, and the scope for the OAuth provider.

  <Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">

  ```js title=src/server/auth/google.js
  export function getConfig() {
    return {
      clientID, // look up from env or elsewhere
      clientSecret, // look up from env or elsewhere
      scope: ['profile', 'email'],
    }
  }
  ```

  </TabItem>
  <TabItem value="ts" label="TypeScript">

  ```ts title=src/server/auth/google.ts
  export function getConfig() {
    return {
      clientID, // look up from env or elsewhere
      clientSecret, // look up from env or elsewhere
      scope: ['profile', 'email'],
    }
  }
  ```

  </TabItem>
  </Tabs>

- #### `getUserFieldsFn: ServerImport`

  This function must return the user fields to use when creating a new user.

  The `context` contains the `User` entity, and the `args` object contains Google profile information.
  You can do whatever you want with this information (e.g., generate a username).

  Here is how to generate a username based on the Google display name:
  <Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">

  ```js title=src/server/auth/google.js
  import { generateAvailableUsername } from '@wasp/core/auth.js'

  export const getUserFields = async (_context, args) => {
    const username = await generateAvailableUsername(
      args.profile.displayName.split(' '),
      { separator: '.' }
    )
    return { username }
  }
  ```

  </TabItem>
  <TabItem value="ts" label="TypeScript">

  ```ts title=src/server/auth/google.ts
  import type { GetUserFieldsFn } from '@wasp/types'
  import { generateAvailableUsername } from '@wasp/core/auth.js'

  export const getUserFields: GetUserFieldsFn = async (_context, args) => {
    const username = await generateAvailableUsername(
      args.profile.displayName.split(' '),
      { separator: '.' }
    )
    return { username }
  }
  ```

  <GetUserFieldsType />

  </TabItem>
  </Tabs>

  <UsernameGenerateExplanation />
