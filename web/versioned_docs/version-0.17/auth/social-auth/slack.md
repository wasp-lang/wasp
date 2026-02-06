---
title: Slack
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
import SlackData from '../entities/\_slack-data.md';
import AccessingUserDataNote from '../\_accessing-user-data-note.md';
import Collapse from '@site/src/components/Collapse';

Wasp supports Slack Authentication out of the box.

Using Slack Authentication is perfect when you build a control panel for a Slack app.

Let's walk through enabling Slack Authentication, explain some quirks, explore default settings and show how to override them.

## Setting up Slack Auth

Enabling Slack Authentication comes down to a series of steps:

1. Enabling Slack authentication in the Wasp file.
2. Adding the `User` entity.
3. Creating Slack App.
4. Adding the necessary Routes and Pages
5. Using Auth UI components in our Pages.

<WaspFileStructureNote />

### 1. Enabling Slack authentication in the Wasp file.

Now let's properly configure the Auth object:

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
      // 2. Enable Slack Auth
      // highlight-next-line
      slack: {}
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

### 3. Creating a Slack App

To use Slack as an authentication method, you'll first need to create a Slack App and provide Wasp with your client key and secret. Here's how you do it:

1. Log into your Slack account and navigate to: https://api.slack.com/apps.
2. Select **Create New App**.
3. Click "From scratch"
3. Enter App Name and select workspace that should host your app.

<img alt="Slack Applications Screenshot" src={useBaseUrl('img/integrations-slack-1.png')} width="400px" />

4. Go to the **OAuth & Permissions** tab on the sidebar and click **Add New Redirect URL**. 
    - Enter the value `https://<subdomain>.local.lt/auth/slack/callback`, where `<subdomain>` is your selected localtunnel subdomain.
    - Slack requires us to use HTTPS even when developing, [read below](#slack-https) how to set it up.

4. Hit **Save URLs**.
5. Go to **Basic Information** tab
6. Hit **Show** next to **Client Secret**
6. Copy your Client ID and Client Secret as you'll need them in the next step.

:::tip

Be precise with your redirect URL. Slack’s redirect URLs are case-sensitive and sensitive to trailing slashes.
For example, `https://your-app.loca.lt/auth/slack/callback` and `https://your-app.loca.lt/auth/slack/callback/` are **not** the same.
:::

### 4. Adding Environment Variables

Add these environment variables to the `.env.server` file at the root of your project (take their values from the previous step):

```bash title=".env.server"
SLACK_CLIENT_ID=your-slack-client-id
SLACK_CLIENT_SECRET=your-slack-client-secret
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

Yay, we've successfully set up Slack Auth! 🎉

![Slack Auth](/img/auth/slack.png)

Running `wasp db migrate-dev` and `wasp start` should now give you a working app with authentication.
To see how to protect specific pages (i.e., hide them from non-authenticated users), read the docs on [using auth](../../auth/overview).

## Developing with Slack auth and HTTPS {#slack-https}

Unlike most OAuth providers, Slack **requires HTTPS and publicly accessible URL for the OAuth redirect URL**.
This means that we can't simply use `localhost:3001` as a base host for redirect urls. Instead, we need to configure
Wasp server to be publicly available under HTTPS, even in the local development environment.

Fortunately, there are quite a few free and convenient tools available to simplify the process, such as
[localtunnel.me](https://localtunnel.me/) (free) and [ngrok.com](https://ngrok.com) (lots of features,
but free tier is limited).

<Collapse title="Using localtunnel">

Install localtunnel globally with `npm install -g localtunnel`. 

Start a tunnel with `lt --port 3001 -s <subdomain>`, where `<subdomain>` is a unique subdomain you would like to have.

:::info Subdomain option

Usually localtunnel will assign you a random subdomain on each start, but you can specify it with the `-s` flag.
Doing it this way will make it easier to remember the URL and will also make it easier to set up the redirect URL
in Slack app settings.

:::

After starting the tunnel, you will see your tunnel URL in the terminal. Go to that URL to unlock the tunnel by entering your IP address
in a field that appears on the page the first time you open it in the browser. This is a basic anti-abuse mechanism. If you're not sure
what your IP is, you can find it by running `curl ifconfig.me` or going to [ifconfig.me](https://ifconfig.me).

Now that your server is exposed to the public, we need to configure Wasp to use the new public domain. This needs to be done in two places:
server and client configuration.

To configure client, add this line to your `.env.client` file (create it if doesn't exist):
```bash title=".env.client"
REACT_APP_API_URL=https://<subdomain>.loca.lt
```

Similarly, to configure the server, add this line to your `.env.server`:
```bash title=".env.server"
WASP_SERVER_URL=https://<subdomain>.loca.lt
```

</Collapse>

## Default Behaviour

Add `slack: {}` to the `auth.methods` dictionary to use it with default settings.

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
      slack: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

<DefaultBehaviour />

## Overrides

<OverrideIntro />

### Data Received From Slack

We are using Slack's API and its `/openid.connect.userInfo` endpoint to get the user data.

The data we receive from Slack on the `/openid.connect.userInfo` endpoint looks something like this:

```json
{
    "ok": true,
    "sub": "U0R7JM",
    "https://slack.com/user_id": "U0R7JM",
    "https://slack.com/team_id": "T0R7GR",
    "email": "krane@slack-corp.com",
    "email_verified": true,
    "date_email_verified": 1622128723,
    "name": "krane",
    "picture": "https://secure.gravatar.com/....png",
    "given_name": "Bront",
    "family_name": "Labradoodle",
    "locale": "en-US",
    "https://slack.com/team_name": "kraneflannel",
    "https://slack.com/team_domain": "kraneflannel",
    "https://slack.com/user_image_24": "...",
    "https://slack.com/user_image_32": "...",
    "https://slack.com/user_image_48": "...",
    "https://slack.com/user_image_72": "...",
    "https://slack.com/user_image_192": "...",
    "https://slack.com/user_image_512": "...",
    "https://slack.com/team_image_34": "...",
    "https://slack.com/team_image_44": "...",
    "https://slack.com/team_image_68": "...",
    "https://slack.com/team_image_88": "...",
    "https://slack.com/team_image_102": "...",
    "https://slack.com/team_image_132": "...",
    "https://slack.com/team_image_230": "...",
    "https://slack.com/team_image_default": true
}
```

The fields you receive depend on the scopes you request. In the example above, the scope includes `email`, `profile` and `openid`. By default, only `openid` is requested. See below for instructions on how to request additional scopes.

<small>
  For an up to date info about the data received from Slack, please refer to the [Slack API documentation](https://api.slack.com/methods/openid.connect.userInfo).
</small>

### Using the Data Received From Slack

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
      slack: {
        // highlight-next-line
        configFn: import { config } from "@src/auth/slack",
        // highlight-next-line
        userSignupFields: import { userSignupFields } from "@src/auth/slack"
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
  avatarUrl   String
}

// ...
```

```ts title="src/auth/slack.ts" auto-js
import { defineUserSignupFields } from 'wasp/server/auth'

export function config() {
  console.log('Inside user-supplied Slack config')
  return {
    scopes: ["openid", "email", "profile"],
  }
}

export const userSignupFields = defineUserSignupFields({
  username: (data: any) => data.profile.name,
  avatarUrl: (data: any) => data.profile.picture,
})
```

<GetUserFieldsType />

## Using Auth

<UsingAuthNote />

When you receive the `user` object [on the client or the server](../overview.md#accessing-the-logged-in-user), you'll be able to access the user's Slack ID like this:

<SlackData />

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
      slack: {
        // highlight-next-line
        configFn: import { config } from "@src/auth/slack",
        // highlight-next-line
        userSignupFields: import { userSignupFields } from "@src/auth/slack"
      }
    },
    onAuthFailedRedirectTo: "/login"
  },
}
```

The `slack` dict has the following properties:

- #### `configFn: ExtImport`

  This function should return an object with the scopes for the OAuth provider.

  ```ts title="src/auth/slack.ts" auto-js
  export function getConfig() {
    return {
      scopes: ["openid", "email", "profile"],
    }
  }
  ```

- #### `userSignupFields: ExtImport`

  <UserSignupFieldsExplainer />

  Read more about the `userSignupFields` function [here](../overview#1-defining-extra-fields).
