---
title: GitHub Integrations
---

import useBaseUrl from '@docusaurus/useBaseUrl';

# GitHub

To implement GitHub Auth, you'll need to add the Auth object with the following configuration to your `main.wasp` file:
```c title="main.wasp"
app Example {
  wasp: {
    version: "^0.8.0"
  },

  title: "Example",

  auth: {
    userEntity: User,
    externalAuthEntity: SocialLogin,
    methods: {
      gitHub: {}
    },
    onAuthFailedRedirectTo: "/login"
  },
}

//...

entity User {=psl
    id          Int     @id @default(autoincrement())
    username    String  @unique
    password    String
    externalAuthAssociations  SocialLogin[]
psl=}

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

For more info on the specific fields, check out this [Auth](../language/features#social-login-providers-oauth-20) section of the docs.

If you're adding a new entity to your `.wasp` file for the first time, make sure to create the table for it in your database:
```shell
wasp db migrate-dev
```

You'll also need to add these environment variables to your `.env.server` file at the root of your project:

```bash title=".env.server"
GITHUB_CLIENT_ID=your-github-client-id
GITHUB_CLIENT_SECRET=your-github-client-secret

JWT_SECRET=random-string-at-least-32-characters-long.
```
We will cover how to get these values in the next section.


## GitHub Auth

To use GitHub as an authentication method (covered [here](/docs/language/features#github)), you'll first need to create a GitHub OAuth App and provide Wasp with your client key and secret. Here is how to do so:

1. Log into your GitHub account and navigate to: https://github.com/settings/developers
2. Select "New OAuth App"
3. Supply required information

  <img alt="GitHub Applications Screenshot"
      src={useBaseUrl('img/integrations-github-1.png')}
      width="400px"
  />

  - For "Authorization callback URL", if you just want to test your local app, put in: `http://localhost:3000/auth/login/github`
  - Once you know on which URL your API server will be deployed, you can create a new app with that URL instead.
      - For example: `https://someotherhost.com/auth/login/github`
4. Hit "Register application"
5. Copy your Client ID and Client secret, and expose them as environment variables named `GITHUB_CLIENT_ID` and `GITHUB_CLIENT_SECRET` wherever your app is running
