---
title: Env Variables
---

import ClientEnvVarsNote from './\_clientEnvVarsNote.md'
import { EnvVarsTable, EnvVar } from './EnvVarsTable'

**Environment variables** are used to configure projects based on the context in which they run. This allows them to exhibit different behaviors in different environments, such as development, staging, or production.

For instance, _during development_, you may want your project to connect to a local development database running on your machine, but _in production_, you want it to connect to the production database. Similarly, in development, you may want to use a test Stripe account, while in production, your app should use a real Stripe account.

While some env vars are required by Wasp, such as the database connection or secrets for social auth, you can also define your env vars for any other useful purposes, and then access them in the code.

Let's go over the available env vars in Wasp, how to define them, and how to use them in your project.

## Client Env Vars {#client-env-vars}

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

Read more about the `env` object in the [API reference](#client-env-vars-api).

### Wasp Client Env Vars

Here are the client env vars that Wasp defines:

#### General Configuration {#client-general-configuration}

These are some general env variables used for various Wasp features:

<EnvVarsTable
  envVars={[
{ name: "REACT_APP_API_URL", type: "URL", isRequired: false, defaultValue: "http://localhost:3001", note: "The client uses this as the server URL." }
]}
/>

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

#### General Configuration {#server-general-configuration}

These are some general env variables used for various Wasp features:

<EnvVarsTable
  envVars={[
{ name: "DATABASE_URL", type: "String", isRequired: true, note: "The URL of the PostgreSQL database you want your app to use." },
{ name: "WASP_WEB_CLIENT_URL", type: "URL", isRequired: true, note: "Server uses this value as your client URL in various features e.g. linking to your app in e-mails." },
{ name: "WASP_SERVER_URL", type: "URL", isRequired: true, note: "Server uses this value as your server URL in various features e.g. to redirect users when logging in with OAuth providers like Google or GitHub." },
{ name: "JWT_SECRET", type: "String", isRequired: true, note: <span>Needed to generate secure tokens. <a href="https://jwtsecret.com/generate" target="_blank" rel="noreferrer">Generate</a> a random string at least 32 characters long.</span> },
{ name: "PORT", type: "Integer", isRequired: false, defaultValue: "3001", note: "This is where the server listens for requests." }
]}
/>

#### SMTP Email Sender

If you are using `SMTP` as your email sender, you need to provide the following environment variables:

<EnvVarsTable
  envVars={[
{ name: "SMTP_HOST", type: "String", isRequired: true, note: "The SMTP server host." },
{ name: "SMTP_PORT", type: "Integer", isRequired: true, note: "The SMTP server port." },
{ name: "SMTP_USERNAME", type: "String", isRequired: true, note: "The SMTP server username." },
{ name: "SMTP_PASSWORD", type: "String", isRequired: true, note: "The SMTP server password." }
]}
/>

#### SendGrid Email Sender

If you are using `SendGrid` as your email sender, you need to provide the following environment variables:

<EnvVarsTable
  envVars={[
{ name: "SENDGRID_API_KEY", type: "String", isRequired: true, note: "The SendGrid API key." }
]}
/>

#### Mailgun Email Sender

If you are using `Mailgun` as your email sender, you need to provide the following environment variables:

<EnvVarsTable
  envVars={[
{ name: "MAILGUN_API_KEY", type: "String", isRequired: true, note: "The Mailgun API key." },
{ name: "MAILGUN_DOMAIN", type: "String", isRequired: true, note: "The Mailgun domain." },
{ name: "MAILGUN_API_URL", type: "URL", isRequired: false, note: <span>Useful if you want to use the EU API endpoint (<code>https://api.eu.mailgun.net</code>).</span> }
]}
/>

#### OAuth Providers

If you are using OAuth, you need to provide the following environment variables:

<EnvVarsTable
  envVars={[
{ name: "<PROVIDER_NAME>_CLIENT_ID", type: "String", isRequired: true, note: "The client ID provided by the OAuth provider." },
{ name: "<PROVIDER_NAME>_CLIENT_SECRET", type: "String", isRequired: true, note: "The client secret provided by the OAuth provider." }
]}
/>

<small>
  \* `<PROVIDER_NAME>` is the uppercase name of the provider you are using. For example, if you are using Google OAuth, you need to provide the `GOOGLE_CLIENT_ID` and `GOOGLE_CLIENT_SECRET` environment variables.
</small>

If you are using [Keycloak](../auth/social-auth/keycloak.md), you'll need to provide one extra environment variable:

<EnvVarsTable
  envVars={[
{ name: "KEYCLOAK_REALM_URL", type: "URL", isRequired: true, note: "The URL of the Keycloak realm." }
]}
/>

#### Jobs

<EnvVarsTable
  envVars={[
{ name: "PG_BOSS_NEW_OPTIONS", type: "String", isRequired: false, note: <span>A <a href="#json-env-vars">JSON env var</a>. Enables you to provide <a href="../advanced/jobs#pg_boss_new_options">custom config</a> for PgBoss.</span> }
]}
/>

#### Development

We provide some helper env variables in development:

<!-- TODO: this not really Boolean, it's really a string that accepts "true" as only true value -->

<EnvVarsTable
  envVars={[
{ name: "SKIP_EMAIL_VERIFICATION_IN_DEV", type: "Boolean", isRequired: false, defaultValue: "false", note: "If set to true, automatically sets user emails as verified in development." }
]}
/>

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

We talk about how to define env vars for each deployment option in the [deployment section](../deployment/env-vars.md).

## JSON Env Vars {#json-env-vars}

Some of the environment variables you pass to Wasp are parsed as JSON values. This is useful for features needing more in-depth configuration, but it comes with the caveat of ensuring that the JSON syntax is valid.

The main issue comes in the form of escaping quotes, and the different ways to do it depending on where you are defining the env var.

#### In `.env` files

In `.env` files, you don't need to quote the full value, so you don't need to escape the quotes. For example, you can define a JSON object like this:

```shell title=".env.server"
PG_BOSS_NEW_OPTIONS={"connectionString":"...db url...","jobExpirationInSeconds":60,"maxRetries":3}
```

#### In the shell

In the shell, you need to quote the full value and escape the quotes inside the JSON object. For example, you can define a JSON object like this:

```shell
PG_BOSS_NEW_OPTIONS="{\"connectionString\":\"...db url...\",\"jobExpirationInSeconds\":60,\"maxRetries\":3}"
```

As an alternative, you can use single quotes to avoid escaping the quotes inside the JSON object:

```shell
PG_BOSS_NEW_OPTIONS='{"connectionString":"...db url...","jobExpirationInSeconds":60,"maxRetries":3}'
```

## Custom Env Var Validations

If your code requires some environment variables, you usually want to ensure that they are correctly defined. In Wasp, you can define your environment variables validation by defining a [Zod object schema](https://zod.dev/?id=basic-usage) and telling Wasp to use it.

:::info What is Zod?

[Zod](https://zod.dev/) is a library that lets you define what you expect from your data. For example, you can use Zod to define that:

- A value should be a string that's a valid email address.
- A value should be a number between 0 and 100.
- ... and much more.

:::

Take a look at an example of defining env vars validation:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="src/env.js"
    import * as z from 'zod'

    import { defineEnvValidationSchema } from 'wasp/env'

    export const serverEnvValidationSchema = defineEnvValidationSchema(
      z.object({
        STRIPE_API_KEY: z.string({
          required_error: 'STRIPE_API_KEY is required.',
        }),
      })
    )

    export const clientEnvValidationSchema = defineEnvValidationSchema(
      z.object({
        REACT_APP_NAME: z.string().default('TODO App'),
      })
    )
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="src/env.ts"
    import * as z from 'zod'

    import { defineEnvValidationSchema } from 'wasp/env'

    export const serverEnvValidationSchema = defineEnvValidationSchema(
      z.object({
        STRIPE_API_KEY: z.string({
          required_error: 'STRIPE_API_KEY is required.',
        }),
      })
    )

    export const clientEnvValidationSchema = defineEnvValidationSchema(
      z.object({
        REACT_APP_NAME: z.string().default('TODO App'),
      })
    )
    ```

    The `defineEnvValidationSchema` function ensures your Zod schema is type-checked.
  </TabItem>
</Tabs>

```wasp title="main.wasp"
app myApp {
  ...
  client: {
    envValidationSchema: import { clientEnvValidationSchema } from "@src/env",
  },
  server: {
    envValidationSchema: import { serverEnvValidationSchema } from "@src/env",
  },
}
```

You defined schemas for both the client and the server env vars and told Wasp to use them. Wasp merges your env validation schemas with the built-in env vars validation schemas when it validates the `process.env` object on the server and the `import.meta.env` object on the client.

This means you can use the `env` object to access **your env vars** like this:

```ts title="src/stripe.ts"
import { env } from 'wasp/server'

const stripeApiKey = env.STRIPE_API_KEY
```

Read more about the env object in the [API Reference](#api-reference).

## API Reference

There are **Wasp-defined** and **user-defined** env vars. Wasp already comes with built-in validation for Wasp-defined env vars. For your env vars, you can define your own validation.

### Client Env Vars {#client-env-vars-api}

#### User-defined env vars validation

You can define your client env vars validation like this:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="src/env.js"
    import * as z from 'zod'

    import { defineEnvValidationSchema } from 'wasp/env'

    export const envValidationSchema = defineEnvValidationSchema(
      z.object({
        REACT_APP_ANALYTICS_ID: z.string({
          required_error: 'REACT_APP_ANALYTICS_ID is required.',
        }),
      })
    )
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="src/env.ts"
    import * as z from 'zod'

    import { defineEnvValidationSchema } from 'wasp/env'

    export const envValidationSchema = defineEnvValidationSchema(
      z.object({
        REACT_APP_ANALYTICS_ID: z.string({
          required_error: 'REACT_APP_ANALYTICS_ID is required.',
        }),
      })
    )
    ```

    The `defineEnvValidationSchema` function ensures your Zod schema is type-checked.
  </TabItem>
</Tabs>

```wasp title="main.wasp"
app myApp {
  ...
  client: {
    envValidationSchema: import { envValidationSchema } from "@src/env",
  },
}
```

Wasp merges your env validation schemas with the built-in env vars validation schemas when it validates the `import.meta.env` object.

#### Accessing env vars in client code

You can access both **Wasp-defined** and **user-defined** client env vars in your client code using the `env` object:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="src/App.js"
    import { env } from 'wasp/client'

    // Wasp-defined
    const apiUrl = env.REACT_APP_API_URL

    // User-defined
    const analyticsId = env.REACT_APP_ANALYTICS_ID
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="src/App.ts"
    import { env } from 'wasp/client'

    // Wasp-defined
    const apiUrl = env.REACT_APP_API_URL

    // User-defined
    const analyticsId = env.REACT_APP_ANALYTICS_ID
    ```
  </TabItem>
</Tabs>

You can use `import.meta.env.REACT_APP_SOME_VAR_NAME` directly in your code. We don't recommend this since `import.meta.env` isn't validated and missing env vars can cause runtime errors.

### Server Env Vars

#### User-defined env vars validation

You can define your env vars validation like this:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="src/env.js"
    import * as z from 'zod'

    import { defineEnvValidationSchema } from 'wasp/env'

    export const envValidationSchema = defineEnvValidationSchema(
      z.object({
        STRIPE_API_KEY: z.string({
          required_error: 'STRIPE_API_KEY is required.',
        }),
      })
    )
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="src/env.ts"
    import * as z from 'zod'

    import { defineEnvValidationSchema } from 'wasp/env'

    export const envValidationSchema = defineEnvValidationSchema(
      z.object({
        STRIPE_API_KEY: z.string({
          required_error: 'STRIPE_API_KEY is required.',
        }),
      })
    )
    ```

    The `defineEnvValidationSchema` function ensures your Zod schema is type-checked.
  </TabItem>
</Tabs>

```wasp title="main.wasp"
app myApp {
  ...
  server: {
    envValidationSchema: import { envValidationSchema } from "@src/env",
  },
}
```

Wasp merges your env validation schemas with the built-in env vars validation schemas when it validates the `process.env` object.

#### Accessing env vars in server code

You can access both **Wasp-defined** and **user-defined** client env vars in your client code using the `env` object:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="src/stripe.js"
    import { env } from 'wasp/server'

    // Wasp-defined
    const serverUrl = env.WASP_SERVER_URL

    // User-defined
    const stripeApiKey = env.STRIPE_API_KEY
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="src/stripe.ts"
    import { env } from 'wasp/server'

    // Wasp-defined
    const serverUrl = env.WASP_SERVER_URL

    // User-defined
    const stripeApiKey = env.STRIPE_API_KEY
    ```
  </TabItem>
</Tabs>

You can use `process.env.SOME_SECRET` directly in your code. We don't recommend this since `process.env` isn't validated and missing env vars can cause runtime errors.
