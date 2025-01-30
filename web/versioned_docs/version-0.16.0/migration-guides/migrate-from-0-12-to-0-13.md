---
title: Migration from 0.12.X to 0.13.X
---

:::note Are you on 0.11.X or earlier?

This guide only covers the migration from **0.12.X to 0.13.X**. If you are migrating from 0.11.X or earlier, please read the [migration guide from 0.11.X to 0.12.X](./migrate-from-0-11-to-0-12.md) first.

Make sure to read the [migration guide from 0.13.X to 0.14.X](./migrate-from-0-13-to-0-14.md) after you finish this one.

:::

## What's new in 0.13.0?

### OAuth providers got an overhaul

Wasp 0.13.0 switches away from using Passport for our OAuth providers in favor of [Arctic](https://arctic.js.org/) from the [Lucia](https://lucia-auth.com/) ecosystem. This change simplifies the codebase and makes it easier to add new OAuth providers in the future.

### We added Keycloak as an OAuth provider

Wasp now supports using [Keycloak](https://www.keycloak.org/) as an OAuth provider.

## How to migrate?

### Migrate your OAuth setup

We had to make some breaking changes to upgrade the OAuth setup to the new Arctic lib.

Follow the steps below to migrate:

1. **Define the `WASP_SERVER_URL` server env variable**

   In 0.13.0 Wasp introduces a new server env variable `WASP_SERVER_URL` that you need to define. This is the URL of your Wasp server and it's used to generate the redirect URL for the OAuth providers.

   ```bash title="Server env variables"
   WASP_SERVER_URL=https://your-wasp-server-url.com
   ```

   In development, Wasp sets the `WASP_SERVER_URL` to `http://localhost:3001` by default.

   :::info Migrating a deployed app

    If you are migrating a deployed app, you will need to define the `WASP_SERVER_URL` server env variable in your deployment environment.

    Read more about setting env variables in production [here](../project/env-vars#defining-env-vars-in-production).
   :::

2. **Update the redirect URLs** for the OAuth providers

    The redirect URL for the OAuth providers has changed. You will need to update the redirect URL for the OAuth providers in the provider's dashboard.

    <Tabs>
    <TabItem value="before" label="Before">

    ```
    {clientUrl}/auth/login/{provider}
    ```
    </TabItem>
    <TabItem value="after" label="After">

    ```
    {serverUrl}/auth/{provider}/callback
    ```
    </TabItem>
    </Tabs>

    Check the new redirect URLs for [Google](../auth/social-auth/google.md#3-creating-a-google-oauth-app) and [GitHub](../auth/social-auth/github.md#3-creating-a-github-oauth-app) in Wasp's docs.

3. **Update the `configFn`** for the OAuth providers

    If you didn't use the `configFn` option, you can skip this step.

    If you used the `configFn` to configure the `scope` for the OAuth providers, you will need to rename the `scope` property to `scopes`.

    Also, the object returned from `configFn` no longer needs to include the Client ID and the Client Secret. You can remove them from the object that `configFn` returns.

    <Tabs>
    <TabItem value="before" label="Before">
    
    ```ts title="google.ts"
    export function getConfig() {
        return {
            clientID: process.env.GOOGLE_CLIENT_ID,
            clientSecret: process.env.GOOGLE_CLIENT_SECRET,
            scope: ['profile', 'email'],
        }
    }
    ```
    </TabItem>

    <TabItem value="after" label="After">

    ```ts title="google.ts"
    export function getConfig() {
        return {
            scopes: ['profile', 'email'],
        }
    }
    ```
    </TabItem>
    </Tabs>

4. **Update the `userSignupFields` fields** to use the new `profile` format

    If you didn't use the `userSignupFields` option, you can skip this step.

    The data format for the `profile` that you receive from the OAuth providers has changed. You will need to update your code to reflect this change.

    <Tabs>
    <TabItem value="before" label="Before">
    
    ```ts title="google.ts"
    import { defineUserSignupFields } from 'wasp/server/auth'

    export const userSignupFields = defineUserSignupFields({
        displayName: (data: any) => data.profile.displayName,
    })
    ```
    </TabItem>
    <TabItem value="after" label="After">

    ```ts title="google.ts"
    import { defineUserSignupFields } from 'wasp/server/auth'

    export const userSignupFields = defineUserSignupFields({
        displayName: (data: any) => data.profile.name,
    })
    ```
    </TabItem>
    </Tabs>

    Wasp now directly forwards what it receives from the OAuth providers. You can check the data format for [Google](../auth/social-auth/google.md#data-received-from-google) and [GitHub](../auth/social-auth/github.md#data-received-from-github) in Wasp's docs.

That's it!

You should now be able to run your app with the new Wasp 0.13.0.