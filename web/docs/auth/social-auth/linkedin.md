---
title: LinkedIn
---

import DefaultBehaviour from './\_default-behaviour.md';
import OverrideIntro from './\_override-intro.md';
import OverrideExampleIntro from './\_override-example-intro.md';
import UsingAuthNote from './\_using-auth-note.md';
import WaspFileStructureNote from './\_wasp-file-structure-note.md';
import GetUserFieldsType from './\_getuserfields-type.md';
import { CardLink } from '@site/src/components/CardLink';
import LinkedInData from '../entities/\_linkedin-data.md';
import AccessingUserDataNote from '../\_accessing-user-data-note.md';
import SocialLoginClientPages from './\_social-login-client-pages.md';

Wasp supports LinkedIn authentication out of the box. Users can log in with an existing LinkedIn account through LinkedIn's OpenID Connect integration.

## Setting up LinkedIn Auth

Setting up LinkedIn authentication takes these steps:

1. Enable LinkedIn authentication in the Wasp file.
2. Add the `User` entity.
3. Create and configure a LinkedIn app.
4. Add the login Route and Page.
5. Use Wasp's Auth UI on the login Page.

<WaspFileStructureNote />

### 1. Add LinkedIn Auth to the Wasp file

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"

export default app({
  name: "myApp",
  wasp: { version: "{latestWaspVersion}" },
  title: "My App",
  auth: {
    userEntity: "User",
    methods: {
      // highlight-next-line
      linkedin: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
  // ...
})
```

### 2. Add the User entity

```prisma title="schema.prisma"
model User {
  id Int @id @default(autoincrement())
  // Add your own fields below
  // ...
}
```

### 3. Create a LinkedIn app

1. Open the [LinkedIn Developer Portal](https://www.linkedin.com/developers/apps) and create an app.
2. On the app's **Products** tab, request access to **Sign in with LinkedIn using OpenID Connect**.
3. On the **Auth** tab, add `http://localhost:3001/auth/linkedin/callback` under **Authorized redirect URLs for your app**.
4. Add your deployed server callback URL too, for example `https://your-server-url.com/auth/linkedin/callback`.
5. Copy the app's Client ID and Client Secret.

Add the credentials to `.env.server` at the root of your project:

```bash title=".env.server"
LINKEDIN_CLIENT_ID=your-linkedin-client-id
LINKEDIN_CLIENT_SECRET=your-linkedin-client-secret
```

Keep the client secret private. The callback URL must match the URL configured in the LinkedIn Developer Portal.

### 4. Add the login Route and Page

```ts title="main.wasp.ts"
import { app, page, route } from "@wasp.sh/spec"
import { LoginPage } from "./src/pages/auth" with { type: "ref" }

export default app({
  // ...
  spec: [
    route("LoginRoute", "/login", page(LoginPage)),
  ],
})
```

### 5. Create the client Page

<SocialLoginClientPages />

Run `wasp db migrate-dev` and `wasp start`. The generated login form will include a LinkedIn button.

## Default behavior

Add `linkedin: {}` to `auth.methods` to use the default settings:

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"

export default app({
  name: "myApp",
  wasp: { version: "{latestWaspVersion}" },
  title: "My App",
  auth: {
    userEntity: "User",
    methods: {
      linkedin: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
  // ...
})
```

<DefaultBehaviour />

Wasp requests the `openid`, `profile`, and `email` scopes by default. LinkedIn may omit the email fields from its response.

## Overrides

<OverrideIntro />

### Data received from LinkedIn

Wasp fetches the user's profile from LinkedIn's `/v2/userinfo` endpoint. A response can contain these fields:

```json
{
  "sub": "782bbtaQ",
  "name": "John Doe",
  "given_name": "John",
  "family_name": "Doe",
  "picture": "https://media.licdn-ei.com/dms/image/example",
  "locale": "en-US",
  "email": "john@example.com",
  "email_verified": true
}
```

See LinkedIn's [OpenID Connect documentation](https://learn.microsoft.com/en-us/linkedin/consumer/integrations/self-serve/sign-in-with-linkedin-v2) for the current response fields. The `email` and `email_verified` fields are optional.

### Use the data received from LinkedIn

<OverrideExampleIntro />

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"
import { getConfig, userSignupFields } from "./src/auth/linkedin" with { type: "ref" }

export default app({
  name: "myApp",
  wasp: { version: "{latestWaspVersion}" },
  title: "My App",
  auth: {
    userEntity: "User",
    methods: {
      linkedin: {
        configFn: getConfig,
        userSignupFields
      }
    },
    onAuthFailedRedirectTo: "/login"
  },
  // ...
})
```

```prisma title="schema.prisma"
model User {
  id          Int     @id @default(autoincrement())
  displayName String
  email       String?
}
```

```ts title="src/auth/linkedin.ts" auto-js
import { defineUserSignupFields } from "wasp/server/auth"

export const userSignupFields = defineUserSignupFields({
  displayName: (data: any) => data.profile.name,
  email: (data: any) => data.profile.email,
})

export function getConfig() {
  return {
    scopes: ["profile", "email"],
  }
}
```

<GetUserFieldsType />

The Arctic client always includes the required `openid` scope. When overriding `scopes`, include `profile` and `email` if your signup fields use those claims.

## Using Auth

<UsingAuthNote />

The `AuthUser` object exposes the user's LinkedIn subject identifier:

<LinkedInData />

<AccessingUserDataNote />

## API reference

<CardLink
  to="../../api/@wasp.sh/spec/interfaces/SocialAuthConfig"
  kind="api"
  title="SocialAuthConfig"
  description="All the options for the linkedin auth method."
/>

For provider-specific behavior, see [Overrides](#overrides). For behavior shared by all social providers, see the [Social Auth overview](./overview.md).
