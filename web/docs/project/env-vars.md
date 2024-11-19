---
title: Env Variables
---

import ClientEnvVarsNote from './\_ClientEnvVarsNote.md'
import { EnvVarsTable, EnvVar } from './EnvVarsTable'

**Environment variables** are used to configure projects based on the context in which they run. This allows them to exhibit different behaviors in different environments, such as development, staging, or production.

For instance, _during development_, you may want your project to connect to a local development database running on your machine, but _in production_, you want it to connect to the production database. Similarly, in development, you may want to use a test Stripe account, while in production, your app should use a real Stripe account.

While some env vars are required by Wasp, such as the database connection or secrets for social auth, you can also define your env vars for any other useful purposes, and then access them in the code.

Let's go over the available env vars in Wasp, how to define them, and how to use them in your project.

## Client Env Vars

Client environment variables are injected into the client Javascript code during the build process, making them public and readable by anyone. Therefore, you should **never store secrets in them** (such as secret API keys, you should store secrets in the server env variables).

<ClientEnvVarsNote />

You can read them from the client code like this:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/App.js"
import { env } from 'wasp/client'

console.log(env.REACT_APP_SOME_VAR_NAME)
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/App.ts"
import { env } from 'wasp/client'

console.log(env.REACT_APP_SOME_VAR_NAME)
```
</TabItem>
</Tabs>

Read more about the `env` object in the [API reference](#client-env-vars-1).

### Wasp Client Env Vars

Here are the client env vars that Wasp defines:

#### General Configuration

<EnvVarsTable>
  <EnvVar name="REACT_APP_API_URL" type="URL" isRequired={false} defaultValue="http://localhost:3001" note="The client uses this as the server URL." />
</EnvVarsTable>

## Server Env Vars

You can store secret values (e.g. secret API keys) in the server env variables since they are not publicly readable. You can define them without any special prefix, such as `SOME_VAR_NAME=...`.

You can read the env vars from server code like this:
<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js
import { env } from 'wasp/server'

console.log(env.SOME_VAR_NAME)
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```ts
import { env } from 'wasp/server'

console.log(env.SOME_VAR_NAME)
```
</TabItem>
</Tabs>

Read more about the `env` object in the [API reference](#server-env-vars-1).

### Wasp Server Env Vars

#### General Configuration

<EnvVarsTable>
    <EnvVar name="DATABASE_URL" type="String" isRequired={true} note="The URL of the PostgreSQL database you want your app to use." />
    <EnvVar name="WASP_WEB_CLIENT_URL" type="URL" isRequired={true} note="Used as your client URL. The server needs to know about it to properly configure Same-Origin Policy (CORS) headers." />
    <EnvVar name="WASP_SERVER_URL" type="URL" isRequired={true} note="Used as your server URL. The server needs it to properly redirect users when logging in with OAuth providers like Google or GitHub." />
    <EnvVar name="JWT_SECRET" type="String" isRequired={true} note={
      <span>Needed to generate secure tokens. <a href="https://jwtsecret.com/generate" target="_blank" rel="noreferrer">Generate</a> a random string at least 32 characters long.
      </span>
    } />
    <EnvVar name="PORT" type="Integer" isRequired={false} defaultValue="3001" note="This is where the server listens for requests." />
</EnvVarsTable>

#### SMTP Email Sender

If you are using `SMTP` as your email sender, you need to provide the following environment variables:

<EnvVarsTable>
    <EnvVar name="SMTP_HOST" type="String" isRequired={true} note="The SMTP server host." />
    <EnvVar name="SMTP_PORT" type="Integer" isRequired={true} note="The SMTP server port." />
    <EnvVar name="SMTP_USERNAME" type="String" isRequired={true} note="The SMTP server username." />
    <EnvVar name="SMTP_PASSWORD" type="String" isRequired={true} note="The SMTP server password." />
</EnvVarsTable>

#### SendGrid Email Sender

If you are using `SendGrid` as your email sender, you need to provide the following environment variables:


<EnvVarsTable>
    <EnvVar name="SENDGRID_API_KEY" type="String" isRequired={true} note="The SendGrid API key." />
</EnvVarsTable>


#### Mailgun Email Sender

If you are using `Mailgun` as your email sender, you need to provide the following environment variables:

<EnvVarsTable>
    <EnvVar name="MAILGUN_API_KEY" type="String" isRequired={true} note="The Mailgun API key." />
    <EnvVar name="MAILGUN_DOMAIN" type="String" isRequired={true} note="The Mailgun domain." />
    <EnvVar name="MAILGUN_API_URL" type="URL" isRequired={false} note={
      <span>Useful if you want to use the EU API endpoint (<code>https://api.eu.mailgun.net</code>).</span>
    } />
</EnvVarsTable>

#### Auth

You can use this env while developing to skip email verification. This is useful when you are testing the email sending functionality in development:

<!-- TODO: this not really Boolean, it's really a string that accepts "true" as only true value -->
<EnvVarsTable>
    <EnvVar name="SKIP_EMAIL_VERIFICATION_IN_DEV" type="Boolean" isRequired={false} defaultValue="false" note="If set to true, automatically sets user emails as verified in development." />
</EnvVarsTable>


#### Jobs

<EnvVarsTable>
    <EnvVar name="PG_BOSS_NEW_OPTIONS" type="String" isRequired={false} note={
        <span>It's parsed as JSON. Enables you to provide <a href="../advanced/jobs#declaring-jobs">custom config</a> for PgBoss.</span>
    } />
</EnvVarsTable>

#### OAuth Configuration

If you are using OAuth, you need to provide the following environment variables:

<EnvVarsTable>
    <EnvVar name="<PROVIDER_NAME>_CLIENT_ID" type="String" isRequired={true} note="The client ID provided by the OAuth provider." />
    <EnvVar name="<PROVIDER_NAME>_CLIENT_SECRET" type="String" isRequired={true} note="The client secret provided by the OAuth provider." />
</EnvVarsTable>

 `<PROVIDER_NAME>` is the uppercase name of the provider you are using. For example, if you are using Google OAuth, you need to provide the `GOOGLE_CLIENT_ID` and `GOOGLE_CLIENT_SECRET` environment variables.

If you are using [Keycloak](../auth/social-auth/keycloak.md), you'll need to provide one extra environment variable:


<EnvVarsTable>
    <EnvVar name="KEYCLOAK_REALM_URL" type="URL" isRequired={true} note="The URL of the Keycloak realm." />
</EnvVarsTable>

## Defining Env Vars in Development

During development (`wasp start`), there are two ways to provide env vars to your Wasp project:
1. Using `.env` files. **(recommended)**
2. Using shell. (useful for overrides)

### 1. Using .env (dotenv) Files

![Env vars usage in development](/img/env/prod_dev_fade.svg)

This is the recommended method for providing env vars to your Wasp project during development.

In the root of your Wasp project you can create two distinct files:
 - `.env.server` for env vars that will be provided to the server.

  Variables are defined in these files in the form of `NAME=VALUE`, for example:
  ```shell title=".env.server"
  DATABASE_URL=postgresql://localhost:5432
  SOME_VAR_NAME=somevalue
  ```

 - `.env.client` for env vars that will be provided to the client.

    Variables are defined in these files in the form of `NAME=VALUE`, for example:
    ```shell title=".env.client"
    REACT_APP_SOME_VAR_NAME=somevalue
    ```

    <ClientEnvVarsNote />

`.env.server` should not be committed to version control as it can contain secrets, while `.env.client` can be versioned as it must not contain any secrets.
By default, in the `.gitignore` file that comes with a new Wasp app, we ignore all dotenv files.

### 2. Using Shell
If you set environment variables in the shell where you run your Wasp commands (e.g., `wasp start`), Wasp will recognize them.

You can set environment variables in the `.profile` or a similar file, which will set them permanently, or you can set them temporarily by defining them at the start of a command (`SOME_VAR_NAME=SOMEVALUE wasp start`).

This is not specific to Wasp and is simply how environment variables can be set in the shell.

Defining environment variables in this way can be cumbersome even for a single project and even more challenging to manage if you have multiple Wasp projects. Therefore, we do not recommend this as a default method for providing environment variables to Wasp projects during development, you should use .env files instead. However, it can be useful for occasionally **overriding** specific environment variables because environment variables set this way **take precedence over those defined in `.env` files**.

## Defining Env Vars in Production

Defining env variables in production will depend on where you are deploying your Wasp project. In general, you will define them via mechanisms that your hosting provider provides.

We'll talk about how to define env vars for each deployment option in the [deployment section](../deployment/env-vars.md).

## Custom Env Var Validations

TODO: when the Zod validation PRs are merged, describe how users can define their own validations.

## API Reference

### Client Env Vars

Access client env vars in your client code using the `env` object like this:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/App.js"
import { env } from 'wasp/client'

console.log(env.REACT_APP_SOME_VAR_NAME)
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/App.ts"
import { env } from 'wasp/client'

console.log(env.REACT_APP_SOME_VAR_NAME)
```
</TabItem>
</Tabs>

The `env` object is a validated object that Wasp provides to access client env vars. 

You can use `import.meta.env.REACT_APP_SOME_VAR_NAME` directly in your code, but it's not recommended because it's not validated and can lead to runtime errors if the env var is not defined.

<!-- TODO: when the Zod validation PRs are merged, describe how users can define their own validations -->

### Server Env Vars

Access server env vars in your server code using the `env` object like this:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/App.js"
import { env } from 'wasp/server'

console.log(env.SOME_SECRET)
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/App.ts"
import { env } from 'wasp/server'

console.log(env.SOME_SECRET)
```
</TabItem>
</Tabs>

The `env` object is a validated object that Wasp provides to access server env vars. 

You can use `process.env.SOME_SECRET` directly in your code, but it's not recommended because it's not validated and can lead to runtime errors if the env var is not defined.

<!-- TODO: when the Zod validation PRs are merged, describe how users can define their own validations -->