---
title: Google
---

import useBaseUrl from '@docusaurus/useBaseUrl';

TODO: write an introduction

## Setting up Google Auth

We'll need to take the following steps to set up Google authentication:
1. Enable Google authentication in the Wasp file
1. Add the entities
1. Create a Google OAuth app
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

### 1. Add Google Auth to Your Wasp File

To implement Google Auth, you'll need to add the Auth object with the following configuration to your `main.wasp` file:

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
      // 3. Enable Google Auth
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
    // 1. Specify the User entity
    userEntity: User,
    // 2. Specify the SocialLogin entity
    externalAuthEntity: SocialLogin,
    methods: {
      // 3. Enable Google Auth
      google: {}
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

### 3. Create a Google OAuth App

To use Google as an authentication method, you'll first need to create a Google project and provide Wasp with your client key and secret. Here is how to do so:

1. Create a Google Cloud Platform account if you do not already have one: https://cloud.google.com/
2. Create and configure a new Google project here: https://console.cloud.google.com/home/dashboard

  ![Google Console Screenshot 1](/img/integrations-google-1.jpg)

  ![Google Console Screenshot 2](/img/integrations-google-2.jpg)

3. Search for **OAuth** in the top bar, click on **OAuth consent screen**

  ![Google Console Screenshot 3](/img/integrations-google-3.jpg)

  - Select what type of app you want, we will go **External**

    ![Google Console Screenshot 4](/img/integrations-google-4.jpg)

  - Fill out applicable information on Page 1

    ![Google Console Screenshot 5](/img/integrations-google-5.jpg)

  - On Page 2, Scopes, you should select `userinfo.profile`. You can optionally search for other things, like `email`.

    ![Google Console Screenshot 6](/img/integrations-google-6.jpg)

    ![Google Console Screenshot 7](/img/integrations-google-7.jpg)

    ![Google Console Screenshot 8](/img/integrations-google-8.jpg)

  - Add any test users you want on Page 3

    ![Google Console Screenshot 9](/img/integrations-google-9.jpg)

4. Next, click **Credentials**

  ![Google Console Screenshot 10](/img/integrations-google-10.jpg)

  - Select **Create Credentials**
  - Select **OAuth client ID**

    ![Google Console Screenshot 11](/img/integrations-google-11.jpg)

  - Complete the form

    ![Google Console Screenshot 12](/img/integrations-google-12.jpg)

  - Under Authorized redirect URIs, put in: `http://localhost:3000/auth/login/google`

    ![Google Console Screenshot 13](/img/integrations-google-13.jpg)

    - Once you know on which URL(s) your API server will be deployed, also add those URL(s)
      - For example: `https://someotherhost.com/auth/login/google`
  - When you save, you can click the Edit icon and your credentials will be shown

    ![Google Console Screenshot 14](/img/integrations-google-14.jpg)

5. Copy your Client ID and Client secret as you will need them in the next step.

### 4. Add Environment Variables

Add these environment variables to your `.env.server` file at the root of your project:

```bash title=".env.server"
GOOGLE_CLIENT_ID=your-google-client-id
GOOGLE_CLIENT_SECRET=your-google-client-secret
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

Yay, now you set up Google Auth! 🎉 Running `wasp db migrate-dev` and then `wasp start` should give you a working app with authentication. If you want to put some of the pages behind authentication, read the [using auth docs](/docs/auth/overview).

TOOD: put a screenshot 

## Default Settings

Add `google: {}` to your `auth.methods` dictionary to use it with default settings

By default, Wasp expects you to set two environment variables to use Google authentication:
- `GOOGLE_CLIENT_ID`
- `GOOGLE_CLIENT_SECRET`

TODO: write what are the default settings

Read [above](#3-create-a-google-oauth-app) how to get these values.

## Overrides

TODO: give examples of using `configFn` and `getUserFieldsFn` 

## Using Auth

To read more about how to set up the logout button and how to get access to the logged-in user in our client and server code, read the [using auth docs](/docs/auth/overview).

## Options Reference

TODO: make sure all the examples are for Google

- ### `configFn`

  This function should return an object with the following shape:

  ```js title=src/server/auth/google.js
  export function config() {
    // ...
    return {
      clientID, // look up from env or elsewhere,
      clientSecret, // look up from env or elsewhere,
      scope: ['profile'] // must include at least 'profile' for Google
    }
  }

  // ...
  ```

  <p>Here is a link to the <a href="https://github.com/wasp-lang/wasp/blob/release/waspc/data/Generator/templates/server/src/routes/auth/passport/google/defaults.js">default implementations</a> as a reference</p>

- ### `getUserFieldsFn`

  This function should return the user fields to use when creating a new user upon their first time logging in with a Social Auth Provider. The context contains a User entity for DB access, and the args are what the OAuth provider responds with. Here is how you could generate a username based on the Google display name. In your model, you could choose to add more attributes and set additional information.
    ```js title=src/server/auth/google.js
    import { generateAvailableUsername } from '@wasp/core/auth.js'

    // ...

    export async function getUserFields(_context, args) {
      const username = await generateAvailableUsername(args.profile.displayName.split(' '), { separator: '.' })
      return { username }
    }
    ```

    Or you could set the optional `displayName` property on the `User` entity instead:
    ```js title=src/server/auth/google.js
    import { generateAvailableDictionaryUsername, generateAvailableUsername } from '@wasp/core/auth.js'

    // ...

    export async function getUserFields(_context, args) {
      const username = await generateAvailableDictionaryUsername()
      const displayName = await generateAvailableUsername(args.profile.displayName.split(' '), { separator: '.' })
      return { username, displayName }
    }
    ```
    - `generateAvailableUsername` takes an array of Strings and an optional separator and generates a string ending with a random number that is not yet in the database. For example, the above could produce something like "Jim.Smith.3984" for a Google user Jim Smith.
    - `generateAvailableDictionaryUsername` generates a random dictionary phrase that is not yet in the database. For example, `nice-blue-horse-27160`.