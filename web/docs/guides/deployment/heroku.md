---
comments: true
---

import { SecretGeneratorBlock } from '../../project/SecretGeneratorBlock'

# Heroku

## Deploy Wasp to Heroku

This guide shows you how to deploy the server and provision a database for it on Heroku. You can check their [pricing page](https://www.heroku.com/pricing) for more information on their plans.

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

The `PORT` env var will also be provided by Heroku, so the ones left to set are the `JWT_SECRET`, `WASP_WEB_CLIENT_URL` and `WASP_SERVER_URL` env vars:

```
heroku config:set --app <app-name> JWT_SECRET=<random_string_at_least_32_characters_long>
heroku config:set --app <app-name> WASP_WEB_CLIENT_URL=<url_of_where_client_will_be_deployed>
heroku config:set --app <app-name> WASP_SERVER_URL=<url_of_where_server_will_be_deployed>
```

We can help you generate a `JWT_SECRET`:<br/><SecretGeneratorBlock />

:::note
If you do not know what your client URL is yet, don't worry. You can set `WASP_WEB_CLIENT_URL` after you deploy your client.
:::

### Deploy the Heroku app

After you have [built the app](../../deployment/deployment-methods/paas.md#1-generating-deployable-code), position yourself in `.wasp/out/` directory:

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

This is it, the backend is deployed at `https://<app-name>-XXXX.herokuapp.com` 🎉

Find out the exact app URL with:

```shell
heroku info --app <app-name>
```

Additionally, you can check out the logs with:

```shell
heroku logs --tail --app <app-name>
```

:::note Using `pg-boss` with Heroku

If you wish to deploy an app leveraging [Jobs](../../advanced/jobs) that use `pg-boss` as the executor to Heroku, you need to set an additional environment variable called [`PG_BOSS_NEW_OPTIONS`](../../advanced/jobs.md#pg_boss_new_options) to `{"connectionString":"<REGULAR_HEROKU_DATABASE_URL>","ssl":{"rejectUnauthorized":false}}`. This is because pg-boss uses the `pg` extension, which does not seem to connect to Heroku over SSL by default, which Heroku requires. Additionally, Heroku uses a self-signed cert, so we must handle that as well.

Read more: https://devcenter.heroku.com/articles/connecting-heroku-postgres#connecting-in-node-js
:::
