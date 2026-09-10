---
comments: true
---

import LastCheckedWithVersionsNotice from "@site/src/components/LastCheckedWithVersionsNotice";
import { SecretGeneratorBlock } from '../../../project/SecretGeneratorBlock'
import { Server, Client, Database } from '../DeploymentTag'

# Heroku

<LastCheckedWithVersionsNotice versions={{ Wasp: "0.24", Heroku: new Date("2026-04-06") }} />

## Deploy Wasp to Heroku <Server /> <Client /> <Database />

This guide shows you how to deploy your Wasp app and provision a database for it on Heroku. You can check their [pricing page](https://www.heroku.com/pricing) for more information on their plans.

<Tabs groupId="deployment-mode">
<TabItem value="single" label="Single deployment">

The Docker image Wasp generates contains both the server and the built client, so one Heroku app is the whole Wasp app.

</TabItem>
<TabItem value="split" label="Split deployment">

The Docker image Wasp generates contains the server, so this Heroku app is your server. You host the client separately, for example on [Netlify](./netlify.md) or [Cloudflare](./cloudflare.md).

</TabItem>
</Tabs>

### Prerequisites

You will need a Heroku account, `heroku` [CLI](https://devcenter.heroku.com/articles/heroku-cli) and `docker` CLI installed to follow these instructions.

Make sure you are logged in with `heroku` CLI. You can check if you are logged in with `heroku whoami`, and if you are not, you can log in with `heroku login`.

### Set up a Heroku app

:::info
You need to do this only once per Wasp app.
:::

Unless you want to deploy to an existing Heroku app, let's create a new Heroku app:

```
heroku create <app-name>
```

Unless you have an external PostgreSQL database that you want to use, let's create a new database on Heroku and attach it to our app:

```
heroku addons:create --app <app-name> heroku-postgresql:essential-0
```

:::caution

We are using the `essential-0` database instance. It's the cheapest database instance Heroku offers and it costs $5/mo.
:::

Heroku will also set `DATABASE_URL` env var for us at this point. If you are using an external database, you will have to set it up yourself.

The `PORT` env var will also be provided by Heroku, so the ones left to set are the `JWT_SECRET` and `WASP_SERVER_URL` env vars:

```
heroku config:set --app <app-name> JWT_SECRET=<random_string_at_least_32_characters_long>
heroku config:set --app <app-name> WASP_SERVER_URL=https://<app-name>.herokuapp.com
```

We can help you generate a `JWT_SECRET`:<br/><SecretGeneratorBlock />

Find out the exact app URL with `heroku info --app <app-name>`.

<Tabs groupId="deployment-mode">
<TabItem value="single" label="Single deployment">

`WASP_SERVER_URL` is the app's public origin. `WASP_WEB_CLIENT_URL` defaults to it, so you don't need to set it.

</TabItem>
<TabItem value="split" label="Split deployment">

`WASP_SERVER_URL` is the server's own origin. You also need `WASP_WEB_CLIENT_URL`, the URL where you host the client:

```
heroku config:set --app <app-name> WASP_WEB_CLIENT_URL=<url_of_where_client_will_be_deployed>
```

If you do not know what your client URL is yet, don't worry. You can set `WASP_WEB_CLIENT_URL` after you deploy your client.

</TabItem>
</Tabs>

### Deploy the Heroku app

After you have [built the app](../../../deployment/deployment-methods/cloud-providers.md#1-generating-deployable-code) (remember to pass any `REACT_APP_*` client env vars to `wasp build`), position yourself in `.wasp/out/` directory:

```shell
cd .wasp/out
```

assuming you were at the root of your Wasp project at that moment.

Log in to Heroku Container Registry:

```shell
heroku container:login
```

Set your app's stack to `container` so we can deploy our app as a Docker container:

```shell
heroku stack:set container --app <app-name>
```

Build the Docker image and push it to Heroku:

```shell
heroku container:push --app <app-name> web
```

App is still not deployed at this point.
This step might take some time, especially the very first time, since there are no cached Docker layers.

Deploy the pushed image and restart the app:

```shell
heroku container:release --app <app-name> web
```

This is it, your app is deployed at `https://<app-name>.herokuapp.com` 🎉

If you use OAuth, register `https://<app-name>.herokuapp.com/auth/<provider>/callback` as the redirect URI with your provider.

In split mode, this URL is your server. Build the client with `REACT_APP_API_URL=https://<app-name>.herokuapp.com npx vite build` and deploy it to a static host such as [Netlify](./netlify.md).

Find out the exact app URL with:

```shell
heroku info --app <app-name>
```

Additionally, you can check out the logs with:

```shell
heroku logs --tail --app <app-name>
```

:::note Using `pg-boss` with Heroku

If you wish to deploy an app leveraging [Jobs](../../../advanced/jobs) that use `pg-boss` as the executor to Heroku, you need to set an additional environment variable called [`PG_BOSS_NEW_OPTIONS`](../../../advanced/jobs.md#pg_boss_new_options) to `{"connectionString":"<REGULAR_HEROKU_DATABASE_URL>","ssl":{"rejectUnauthorized":false}}`. This is because pg-boss uses the `pg` extension, which does not seem to connect to Heroku over SSL by default, which Heroku requires. Additionally, Heroku uses a self-signed cert, so we must handle that as well.

Read more: https://devcenter.heroku.com/articles/connecting-heroku-postgres#connecting-in-node-js
:::
