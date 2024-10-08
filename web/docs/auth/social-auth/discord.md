---
title: Discord
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
import DiscordData from '../entities/\_discord-data.md';
import AccessingUserDataNote from '../\_accessing-user-data-note.md';

Wasp supports Discord Authentication out of the box.

Letting your users log in using their Discord accounts turns the signup process into a breeze.

Let's walk through enabling Discord Authentication, explain some of the default settings, and show how to override them.

## Setting up Discord Auth

Enabling Discord Authentication comes down to a series of steps:

1. Enabling Discord authentication in the Wasp file.
1. Adding the `User` entity.
1. Creating a Discord App.
1. Adding the necessary Routes and Pages
1. Using Auth UI components in our Pages.

<WaspFileStructureNote />

### 1. Adding Discord Auth to Your Wasp File

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
    // highlight-next-line
    // 1. Specify the User entity  (we'll define it next)
    // highlight-next-line
    userEntity: User,
    methods: {
      // highlight-next-line
      // 2. Enable Discord Auth
      // highlight-next-line
      discord: {}
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
    // highlight-next-line
    // 1. Specify the User entity  (we'll define it next)
    // highlight-next-line
    userEntity: User,
    methods: {
      // highlight-next-line
      // 2. Enable Discord Auth
      // highlight-next-line
      discord: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

</TabItem>
</Tabs>

### 2. Add the User Entity

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

### 3. Creating a Discord App

To use Discord as an authentication method, you'll first need to create a Discord App and provide Wasp with your client key and secret. Here's how you do it:

1. Log into your Discord account and navigate to: https://discord.com/developers/applications.
2. Select **New Application**.
3. Supply required information.

<img alt="Discord Applications Screenshot"
src={useBaseUrl('img/integrations-discord-1.png')}
width="400px"
/>

4. Go to the **OAuth2** tab on the sidebar and click **Add Redirect**

- For development, put: `http://localhost:3001/auth/discord/callback`.
- Once you know on which URL your API server will be deployed, you can create a new app with that URL instead e.g. `https://your-server-url.com/auth/discord/callback`.

4. Hit **Save Changes**.
5. Hit **Reset Secret**.
6. Copy your Client ID and Client secret as you'll need them in the next step.

### 4. Adding Environment Variables

Add these environment variables to the `.env.server` file at the root of your project (take their values from the previous step):

```bash title=".env.server"
DISCORD_CLIENT_ID=your-discord-client-id
DISCORD_CLIENT_SECRET=your-discord-client-secret
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

### 6. Creating the Client Pages

:::info
We are using [Tailwind CSS](https://tailwindcss.com/) to style the pages. Read more about how to add it [here](../../project/css-frameworks).
:::

Let's create a `auth.{jsx,tsx}` file in the `src/pages` folder and add the following to it:

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

We imported the generated Auth UI components and used them in our pages. Read more about the Auth UI components [here](../../auth/ui).

### Conclusion

Yay, we've successfully set up Discord Auth! 🎉

![Discord Auth](/img/auth/discord.png)

Running `wasp db migrate-dev` and `wasp start` should now give you a working app with authentication.
To see how to protect specific pages (i.e., hide them from non-authenticated users), read the docs on [using auth](../../auth/overview).

## Default Behaviour

Add `discord: {}` to the `auth.methods` dictionary to use it with default settings.

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
      discord: {}
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
      discord: {}
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

### Data Received From Discord

We are using Discord's API and its `/users/@me` endpoint to get the user data.

The data we receive from Discord on the `/users/@me` endpoint looks something like this:

```json
{
  "id": "80351110224678912",
  "username": "Nelly",
  "discriminator": "1337",
  "avatar": "8342729096ea3675442027381ff50dfe",
  "verified": true,
  "flags": 64,
  "banner": "06c16474723fe537c283b8efa61a30c8",
  "accent_color": 16711680,
  "premium_type": 1,
  "public_flags": 64,
  "avatar_decoration_data": {
    "sku_id": "1144058844004233369",
    "asset": "a_fed43ab12698df65902ba06727e20c0e"
  }
}
```

The fields you receive will depend on the scopes you requested. The default scope is set to `identify` only. If you want to get the email, you need to specify the `email` scope in the `configFn` function.

<small>

For an up to date info about the data received from Discord, please refer to the [Discord API documentation](https://discord.com/developers/docs/resources/user#user-object-user-structure).
</small>

### Using the Data Received From Discord

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
      discord: {
        // highlight-next-line
        configFn: import { getConfig } from "@src/auth/discord.js",
        // highlight-next-line
        userSignupFields: import { userSignupFields } from "@src/auth/discord.js"
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

```js title=src/auth/discord.js
export const userSignupFields = {
  username: (data) => data.profile.global_name,
  avatarUrl: (data) => data.profile.avatar,
}

export function getConfig() {
  return {
    scopes: ['identify'],
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
      discord: {
        // highlight-next-line
        configFn: import { getConfig } from "@src/auth/discord.js",
        // highlight-next-line
        userSignupFields: import { userSignupFields } from "@src/auth/discord.js"
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

```ts title=src/auth/discord.ts
import { defineUserSignupFields } from 'wasp/server/auth'

export const userSignupFields = defineUserSignupFields({
  username: (data: any) => data.profile.global_name,
  avatarUrl: (data: any) => data.profile.avatar,
})

export function getConfig() {
  return {
    scopes: ['identify'],
  }
}
```

<GetUserFieldsType />

</TabItem>
</Tabs>

## Using Auth

<UsingAuthNote />

When you receive the `user` object [on the client or the server](../overview.md#accessing-the-logged-in-user), you'll be able to access the user's Discord ID like this:

<DiscordData />

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
      discord: {
        // highlight-next-line
        configFn: import { getConfig } from "@src/auth/discord.js",
        // highlight-next-line
        userSignupFields: import { userSignupFields } from "@src/auth/discord.js"
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
      discord: {
        // highlight-next-line
        configFn: import { getConfig } from "@src/auth/discord.js",
        // highlight-next-line
        userSignupFields: import { userSignupFields } from "@src/auth/discord.js"
      }
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

</TabItem>
</Tabs>

The `discord` dict has the following properties:

- #### `configFn: ExtImport`

  This function should return an object with the scopes for the OAuth provider.

  <Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">

  ```js title=src/auth/discord.js
  export function getConfig() {
    return {
      scopes: [],
    }
  }
  ```

  </TabItem>
  <TabItem value="ts" label="TypeScript">

  ```ts title=src/auth/discord.ts
  export function getConfig() {
    return {
      scopes: [],
    }
  }
  ```

  </TabItem>
  </Tabs>

- #### `userSignupFields: ExtImport`

  <UserSignupFieldsExplainer />

  Read more about the `userSignupFields` function [here](../overview#1-defining-extra-fields).
