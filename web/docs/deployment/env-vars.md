---
title: Env Variables
---

We talked about environment variables in the [project setup section](../project/env-vars.md). If you haven't read it, make sure to check it out first. In this section, we'll talk about environment variables in the context of deploying the app.

While developing our app on our machine, we had the option of using `.env.client` and `.env.server` files which made it easy to define and manage env vars.

However, when we are deploying our app, **`.env.client` and `.env.server` files will be ignored, and we need to provide env vars differently.**

![Env vars usage in development and production](/img/env/prod_dev_fade_2.svg)

### Client Env Vars {#client-env-vars}

During the build process, client env vars are injected into the client Javascript code, making them public and readable by anyone. Therefore, you should **never store secrets in them** (such as secret API keys).

When building for production, the `.env.client` file will be ignored, since it is meant to be used only during development.
Instead, you should provide the production client env vars directly to the build command that turns client code into static files.

Make sure to check the [required client env vars](../project/env-vars.md#client-general-configuration) and set them when building for production, the build will fail if any required env vars are missing.

```shell
REACT_APP_API_URL=<url_to_wasp_backend> REACT_APP_SOME_OTHER_VAR_NAME=someothervalue npx vite build
```

Also, notice **that you can't and shouldn't provide client env vars to the client code by setting them on the hosting provider** (unlike providing server env vars to the server app, in that case this is how you should do it). Your client code will ignore those, as at that point client code is just static files.

:::info How it works
What happens behind the scenes is that Wasp will replace all occurrences of `import.meta.env.REACT_APP_SOME_VAR_NAME` in your client code with the env var value you provided. This is done during the build process, so the value is injected into the static files produced from the client code.

Read more about it in Vite's [docs](https://vitejs.dev/guide/env-and-mode.html#production-replacement).
:::

### Server Env Vars

When building your Wasp app for production `.env.server` will be ignored, since it is meant to be used only during development.

You can provide production env vars to your server code in production by defining them and making them available on the server where your server code is running.

::::caution Set the required env vars

Make sure to go through [all the required server env vars](../project/env-vars.md#server-general-configuration) like `DATABASE_URL`, `WASP_WEB_CLIENT_URL`, `WASP_SERVER_URL` etc. and set them up in your production environment.

While some env vars like `WASP_WEB_CLIENT_URL` and `WASP_SERVER_URL` have default values in development, they are **required in production** and must be explicitly set.

**If you are using the [Wasp CLI](./deployment-methods/wasp-deploy/overview.md)** deployment method, Wasp will set the general configuration env vars for you, but you will need to set the rest of the env vars yourself (like the ones for OAuth auth methods or any other custom env vars you might have defined).
::::

Setting server env variables up will highly depend on where you are deploying your server, but in general it comes down to defining the env vars via mechanisms that your hosting provider provides.

For example, if you deploy your server to [Fly](https://fly.io), you can define them using the `fly` CLI tool:

```shell
fly secrets set SOME_VAR_NAME=somevalue
```

We talk about specific providers in the [PaaS deployment section](./deployment-methods/paas.md) or the [self-hosted deployment section](./deployment-methods/self-hosted.md).
