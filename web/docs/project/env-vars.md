---
title: Env Variables
---

**Environment variables** are used to configure projects based on the context in which they run. This allows them to exhibit different behaviors in different environments, such as development, staging, or production.

For instance, _during development_, you may want your project to connect to a local development database running on your machine, but _in production_, you may prefer it to connect to the production database. Similarly, in development, you may want to use a test Stripe account, while in production, your app should use a real Stripe account.

While some env vars are required by Wasp, such as the database connection or secrets for social auth, you can also define your env vars for any other useful purposes, and then access them in the code.

In Wasp, you can use environment variables in both the client and the server code.
## Client Env Vars

Client environment variables are embedded into the client code during the build and shipping process, making them public and readable by anyone. Therefore, you should **never store secrets in them** (such as secret API keys -> you can provide those to server instead).

To enable Wasp to pick them up, client env vars must be prefixed with `REACT_APP_`, for example: `REACT_APP_SOME_VAR_NAME=...`.

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

In server environment variables, you can store secret values (e.g. secret API keys) since they are not publicly readable. You can define them without any special prefix, such as `SOME_VAR_NAME=...`.

You can read the env vars from server code like this:
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

`.env.server` should not be committed to version control as it can contain secrets, while `.env.client` can be versioned as it must not contain any secrets.
By default, in the `.gitignore` file that comes with a new Wasp app, we ignore all dotenv files.

:::info Dotenv files
  `dotenv` files are a popular method for storing configuration: to learn more about them in general, check out the [dotenv npm package](https://www.npmjs.com/package/dotenv).
:::

### 2. Using Shell
If you set environment variables in the shell where you run your Wasp commands (e.g., `wasp start`), Wasp will recognize them.

You can set environment variables in the `.profile` or a similar file, which will set them permanently, or you can set them temporarily by defining them at the start of a command (`SOME_VAR_NAME=SOMEVALUE wasp start`).

This is not specific to Wasp and is simply how environment variables can be set in the shell.

Defining environment variables in this way can be cumbersome even for a single project and even more challenging to manage if you have multiple Wasp projects. Therefore, we do not recommend this as a default method for providing environment variables to Wasp projects during development, you should use .env files instead. However, it can be useful for occasionally **overriding** specific environment variables because environment variables set this way **take precedence over those defined in `.env` files**.

## Defining Env Vars in Production

While in development, we had the option of using `.env.client` and `.env.server` files which made it easy to define and manage env vars.
However, for production, `.env.client` and `.env.server` files will be ignored, and we need to provide env vars differently.

![Env vars usage in development and production](/img/env/prod_dev_fade_2.svg)

### Client Env Vars

Client env vars are embedded into the client code during the build process, making them public and readable by anyone. Therefore, you should **never store secrets in them** (such as secret API keys).

When building for production `.env.client` will be ignored, since it is meant to be used only during development.
Instead, you should provide the production client env vars directly to the build command that turns client code into static files:
```shell
REACT_APP_SOME_VAR_NAME=somevalue REACT_APP_SOME_OTHER_VAR_NAME=someothervalue npm run build
```

Check the [deployment docs](../advanced/deployment/manually#3-deploying-the-web-client-frontend) for more details.

Also, notice that you can't and shouldn't provide env vars to the client code by setting them on the hosting provider where you deployed them (unlike server env vars, where this is how you should do it). Your client code will ignore those, as at that point client code is just static files.

:::info How it works
What happens behind the scenes is that Wasp will replace all occurrences of `import.meta.env.REACT_APP_SOME_VAR_NAME` in your client code with the env var value you provided. This is done during the build process, so the value is embedded into the static files produced from the client code.

Read more about it in Vite's [docs](https://vitejs.dev/guide/env-and-mode.html#production-replacement).
:::

### Server Env Vars

When building for production `.env.server` will be ignored, since it is meant to be used only during development.

You can provide production env vars to your server code in production by defining them and making them available on the server where your server code is running.

Setting this up will highly depend on where you are deploying your Wasp project, but in general it comes down to defining the env vars via mechanisms that your hosting provider provides.

For example, if you deploy your project to [Fly](https://fly.io), you can define them using the `flyctl` CLI tool:

```shell
flyctl secrets set SOME_VAR_NAME=somevalue
```

You can read a lot more details in the [deployment section](../advanced/deployment/manually) of the docs. We go into detail on how to define env vars for each deployment option.
