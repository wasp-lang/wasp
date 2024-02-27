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

Wasp supports Github Authentication out of the box.
GitHub is a great external auth choice when you're building apps for developers, as most of them already have a GitHub account.

Letting your users log in using their GitHub accounts turns the signup process into a breeze.

Let's walk through enabling Github Authentication, explain some of the default settings, and show how to override them.

## Setting up Github Auth

Enabling GitHub Authentication comes down to a series of steps:

1. Enabling GitHub authentication in the Wasp file.
1. Adding the `User` entity.
1. Creating a GitHub OAuth app.
1. Adding the neccessary Routes and Pages
1. Using Auth UI components in our Pages.

<WaspFileStructureNote />

### 1. Adding Github Auth to Your Wasp File

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
    // 1. Specify the User entity  (we'll define it next)
    // highlight-next-line
    userEntity: User,
    methods: {
      // highlight-next-line
      // 2. Enable Github Auth
      // highlight-next-line
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
    // highlight-next-line
    // 1. Specify the User entity  (we'll define it next)
    // highlight-next-line
    userEntity: User,
    methods: {
      // highlight-next-line
      // 2. Enable Github Auth
      // highlight-next-line
      gitHub: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

</TabItem>
</Tabs>

### 2. Add the User Entity

Let's now define the `app.auth.userEntity` entity:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...
// 3. Define the User entity
// highlight-next-line
entity User {=psl
    id          Int     @id @default(autoincrement())
    // ...
psl=}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...
// 3. Define the User entity
// highlight-next-line
entity User {=psl
    id          Int     @id @default(autoincrement())
    // ...
psl=}
```

</TabItem>
</Tabs>

### 3. Creating a GitHub OAuth App

To use GitHub as an authentication method, you'll first need to create a GitHub OAuth App and provide Wasp with your client key and secret. Here's how you do it:

1. Log into your GitHub account and navigate to: https://github.com/settings/developers.
2. Select **New OAuth App**.
3. Supply required information.

<img alt="GitHub Applications Screenshot"
src={useBaseUrl('img/integrations-github-1.png')}
width="400px"
/>

- For **Authorization callback URL**:
  - For development, put: `http://localhost:3000/auth/login/github`.
  - Once you know on which URL your app will be deployed, you can create a new app with that URL instead e.g. `https://someotherhost.com/auth/login/github`.

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

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
// ...

// 6. Define the routes
route LoginRoute { path: "/login", to: LoginPage }
page LoginPage {
  component: import { Login } from "@src/pages/auth.jsx"
}
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
// ...

// 6. Define the routes
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

We imported the generated Auth UI component and used them in our pages. Read more about the Auth UI components [here](../../auth/ui).

### Conclusion

Yay, we've successfully set up Github Auth! 🎉

![Github Auth](/img/auth/github.png)

Running `wasp db migrate-dev` and `wasp start` should now give you a working app with authentication.
To see how to protect specific pages (i.e., hide them from non-authenticated users), read the docs on [using auth](../../auth/overview).

## Default Behaviour

Add `gitHub: {}` to the `auth.methods` dictionary to use it with default settings.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title=main.wasp
app myApp {
  wasp: {
    version: "^0.11.0"
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

</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title=main.wasp
app myApp {
  wasp: {
    version: "^0.11.0"
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

</TabItem>
</Tabs>

<DefaultBehaviour />

## Overrides

<OverrideIntro />

### Using the User's Provider Account Details

<OverrideExampleIntro />

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.11.0"
  },
  title: "My App",
  auth: {
    userEntity: User,
    methods: {
      gitHub: {
        // highlight-next-line
        configFn: import { getConfig } from "@src/auth/github.js",
        // highlight-next-line
        userSignupFields: import { userSignupFields } from "@src/auth/github.js"
      }
    },
    onAuthFailedRedirectTo: "/login"
  },
}

entity User {=psl
    id                        Int     @id @default(autoincrement())
    username                  String  @unique
    displayName               String
psl=}

// ...
```

```js title=src/auth/github.js
export const userSignupFields = {
  username: () => "hardcoded-username",
  displayName: (data) => data.profile.displayName,
};

export function getConfig() {
  return {
    clientID // look up from env or elsewhere
    clientSecret // look up from env or elsewhere
    scope: [],
  };
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
    userEntity: User,
    methods: {
      gitHub: {
        // highlight-next-line
        configFn: import { getConfig } from "@src/auth/github.js",
        // highlight-next-line
        userSignupFields: import { userSignupFields } from "@src/auth/github.js"
      }
    },
    onAuthFailedRedirectTo: "/login"
  },
}

entity User {=psl
    id                        Int     @id @default(autoincrement())
    username                  String  @unique
    displayName               String
psl=}

// ...
```

```ts title=src/auth/github.ts
import { defineUserSignupFields } from 'wasp/server/auth'

export const userSignupFields = defineUserSignupFields({
  username: () => "hardcoded-username",
  displayName: (data) => data.profile.displayName,
})

export function getConfig() {
  return {
    clientID, // look up from env or elsewhere
    clientSecret, // look up from env or elsewhere
    scope: [],
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

```wasp title="main.wasp"
app myApp {
  wasp: {
    version: "^0.11.0"
  },
  title: "My App",
  auth: {
    userEntity: User,
    methods: {
      gitHub: {
        // highlight-next-line
        configFn: import { getConfig } from "@src/auth/github.js",
        // highlight-next-line
        userSignupFields: import { userSignupFields } from "@src/auth/github.js"
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
    version: "^0.11.0"
  },
  title: "My App",
  auth: {
    userEntity: User,
    methods: {
      gitHub: {
        // highlight-next-line
        configFn: import { getConfig } from "@src/auth/github.js",
        // highlight-next-line
        userSignupFields: import { userSignupFields } from "@src/auth/github.js"
      }
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

</TabItem>
</Tabs>

The `gitHub` dict has the following properties:

- #### `configFn: ExtImport`

  This function should return an object with the Client ID, Client Secret, and scope for the OAuth provider.

  <Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">

  ```js title=src/auth/github.js
  export function getConfig() {
    return {
      clientID, // look up from env or elsewhere
      clientSecret, // look up from env or elsewhere
      scope: [],
    }
  }
  ```

  </TabItem>
  <TabItem value="ts" label="TypeScript">

  ```ts title=src/auth/github.ts
  export function getConfig() {
    return {
      clientID, // look up from env or elsewhere
      clientSecret, // look up from env or elsewhere
      scope: [],
    }
  }
  ```

  </TabItem>
  </Tabs>

- #### `userSignupFields: ExtImport`

  <UserSignupFieldsExplainer />
  Read more about the `userSignupFields` function [here](../overview#1-defining-extra-fields).
