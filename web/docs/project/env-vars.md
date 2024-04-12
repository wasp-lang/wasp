---
title: Env Variables
---

**Environment variables** are used to configure projects based on the context in which they run. This allows them to exhibit different behaviors in different environments, such as development, staging, or production.

For instance, _during development_, you may want your project to connect to a local development database running on your machine, but _in production_, you may prefer it to connect to the production database. Similarly, in development, you may want to use a test Stripe account, while in production, your app should use a real Stripe account.

While some env vars are required by Wasp, such as the database connection or secrets for social auth, you can also define your env vars for any other useful purposes.

In Wasp, you can use environment variables in both the client and the server code.
## Client Env Vars

Client environment variables are embedded into the client code during the build and shipping process, making them public and readable by anyone. Therefore, you should **never store secrets in them** (such as secret API keys).

To enable Wasp to pick them up, client environment variables must be prefixed with `REACT_APP_`, for example: `REACT_APP_SOME_VAR_NAME=...`.

You can read them from the client code like this:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/App.js"
console.log(import.meta.env.REACT_APP_SOME_VAR_NAME)
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/App.ts"
console.log(import.meta.env.REACT_APP_SOME_VAR_NAME)
```
</TabItem>
</Tabs>


Check below on how to define them.

## Server Env Vars

In server environment variables, you can store secret values (e.g. secret API keys) since are not publicly readable. You can define them without any special prefix, such as `SOME_VAR_NAME=...`.

You can read them in the server code like this:
<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js
console.log(process.env.SOME_VAR_NAME)
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```ts
console.log(process.env.SOME_VAR_NAME)
```
</TabItem>
</Tabs>

Check below on how to define them.

## Defining Env Vars in Development

During development, there are two ways to provide env vars to your Wasp project:
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

These files should not be committed to version control, and they are already ignored by default in the `.gitignore` file that comes with Wasp.

<!-- `dotenv` files are a popular method for storing configuration: to learn more about them in general, check out the [README of the lib we use for them](https://github.com/stackbuilders/dotenv-hs). -->

### 2. Using Shell
If you set environment variables in the shell where you run your Wasp commands (e.g., `wasp start`), Wasp will recognize them.

You can set environment variables in the `.profile` or a similar file, or by defining them at the start of a command:

```shell
SOME_VAR_NAME=SOMEVALUE wasp start
```

 This is not specific to Wasp and is simply how environment variables can be set in the shell.

Defining environment variables in this way can be cumbersome even for a single project and even more challenging to manage if you have multiple Wasp projects. Therefore, we do not recommend this as a default method for providing environment variables to Wasp projects. However, it can be useful for occasionally **overriding** specific environment variables because environment variables set this way **take precedence over those defined in `.env` files**.

## Defining Env Vars in Production

![Env vars usage in development and production](/img/env/prod_dev_fade_2.svg)

While in development, we had the option of using `.env.client` and `.env.server` files which made it easy to define and manage env vars.
However, for production, `.env.client` and `.env.server` files will be ignored, and we need to provide env vars differently.

### Client Env Vars

To set client env vars for production, you need to ensure they are set for the terminal session in which you are running the build command, for example:
```shell
REACT_APP_SOME_VAR_NAME=somevalue npm run build
```

These client env vars are then embedded into the client code during the build and shipping process, making them public and readable by anyone. Therefore, you should **never store secrets in them** (such as secret API keys).

:::info How it works
What happens behind the scenes is that Wasp will replace all occurrences of `import.meta.env.REACT_APP_SOME_VAR_NAME` with the value you provided. This is done during the build process, so the value is embedded into the client code.

Read more about it in Vite's [docs](https://vitejs.dev/guide/env-and-mode.html#production-replacement).
:::

### Server Env Vars

The way you provide env vars to your Wasp project in production depends on where you deploy it. For example, if you deploy your project to [Fly](https://fly.io), you can define them using the `flyctl` CLI tool:

```shell
flyctl secrets set SOME_VAR_NAME=somevalue
```

You can read a lot more details in the [deployment section](../advanced/deployment/manually) of the docs. We go into detail on how to define env vars for each deployment option.
