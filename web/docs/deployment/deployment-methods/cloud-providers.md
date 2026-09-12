---
title: Cloud Providers
---

import BuildingTheWebClient from './_building-the-web-client.md'
import { CardLink } from '@site/src/components/CardLink'

You can deploy the built Wasp app wherever and however you want, as long as your provider/server supports running a Node.js server (or a Docker image) and running a PostgreSQL database.

## Guides

We have step-by-step guides for deploying your Wasp app to some of the most popular providers:

<CardLink to="../../guides/deployment/cloud-providers/vercel" kind="guide" title="Deploying Wasp to Vercel" description="Uses Vercel, Vercel CLI, Supabase integration" />

<CardLink to="../../guides/deployment/cloud-providers/cloudflare" kind="guide" title="Deploying Wasp to Cloudflare Workers" description="Uses Cloudflare Workers, Wrangler CLI" />

<CardLink to="../../guides/deployment/cloud-providers/flyio" kind="guide" title="Deploying Wasp to Fly.io" description="Uses Fly.io, fly CLI, Docker" />

<CardLink to="../../guides/deployment/cloud-providers/heroku" kind="guide" title="Deploying Wasp to Heroku" description="Uses Heroku, heroku CLI, Docker" />

<CardLink to="../../guides/deployment/cloud-providers/netlify" kind="guide" title="Deploying Wasp to Netlify" description="Uses Netlify, Netlify CLI" />

<CardLink to="../../guides/deployment/cloud-providers/railway" kind="guide" title="Deploying Wasp to Railway" description="Uses Railway, Railway CLI" />

<CardLink to="../../guides/deployment/cloud-providers/render" kind="guide" title="Deploying Wasp on Render" description="Uses Render, Blueprint (IaC)" />

If your desired provider isn't on the list, no worries, you can still deploy your app  - it just means we don't yet have a step-by-step guide for you to follow.
Feel free to [open a PR](https://github.com/wasp-lang/wasp/new/release/web/docs/guides/deployment/cloud-providers) if you'd like to write one yourself :)

## Manual deployment

Deploying a Wasp app comes down to the following:

1. Generating deployable code.
2. Deploying the app.
3. Deploying a PostgreSQL database and keeping it running.

What exactly steps 1 and 2 produce depends on your [deployment mode](../intro.md#deployment-modes), so pick yours below and the steps will follow it.

### 1. Generating Deployable Code

Running the command `wasp build` generates deployable code for your app in the `.wasp/out/` directory. It needs your dependencies installed, so run `wasp install` first.

:::caution PostgreSQL in production
You won't be able to build the app if you are using SQLite as a database (which is the default database).
You'll have to [switch to PostgreSQL](../../data-model/databases.md#migrating-from-sqlite-to-postgresql) before deploying to production.
:::

<Tabs groupId="deployment-mode">
<TabItem value="single" label="Single deployment">

`wasp build` also builds the client into static files in `.wasp/out/web-app/build`, so give it any [client env vars](../env-vars.md#client-env-vars) your app uses:

```
REACT_APP_SOME_VAR=somevalue wasp build
```

</TabItem>
<TabItem value="split" label="Split deployment">

`wasp build` builds only the server:

```
wasp build
```

You build the client yourself, with `REACT_APP_API_URL` set to the origin of the server you are about to deploy:

<BuildingTheWebClient />

The command above puts the client in `.wasp/out/web-app/build`, including the `200.html` file at the root that acts as the SPA fallback.

</TabItem>
</Tabs>

### 2. Deploying the App

<Tabs groupId="deployment-mode">
<TabItem value="single" label="Single deployment">

There's a Dockerfile in the `.wasp/out` directory that defines an image with the server and the built client. The server serves the client's files next to Wasp's own routes (`/auth`, `/operations`, ...), so the whole app is reachable on one URL.

To run the app in production, deploy this Docker image to a hosting provider and make sure the required env variables are correctly set up. Usually, you use the provider's dashboard UI or a CLI tool to set up these env variables.

Check the [required server env variables](../env-vars.md#server-env-vars) and make sure they are set up for your app. `WASP_SERVER_URL` is the app's public origin, and you can leave `WASP_WEB_CLIENT_URL` out.

If your provider checks the app's health, point the check at `/health`, which returns `200` with `{"status":"ok"}`. `GET /` also returns `200`, but it serves your app's HTML.

</TabItem>
<TabItem value="split" label="Split deployment">

There's a Dockerfile in the `.wasp/out` directory that defines an image with the server. Deploy this Docker image to a hosting provider that runs Node.js apps or Docker images, and make sure the required env variables are correctly set up. Usually, you use the provider's dashboard UI or a CLI tool to set up these env variables.

Check the [required server env variables](../env-vars.md#server-env-vars) and make sure they are set up for your server. `WASP_SERVER_URL` is the server's own origin, and `WASP_WEB_CLIENT_URL` has to be the client's URL, so that CORS, e-mail links and OAuth redirects point at it.

The client is just a bunch of static files, so you can deploy the contents of `.wasp/out/web-app/build` to any static hosting provider, for example [Netlify](../../guides/deployment/cloud-providers/netlify.md) or [Cloudflare](../../guides/deployment/cloud-providers/cloudflare.md).

If your provider checks the server's health, point the check at `/health`, which returns `200` with `{"status":"ok"}`.

</TabItem>
</Tabs>

While these are the general instructions on deploying the app anywhere, we also have more detailed instructions for chosen providers above, so check that out for more guidance if you are deploying to one of those providers.

### 3. Deploying the Database

Any PostgreSQL database will do, as long as you provide the server with the correct `DATABASE_URL` env var and ensure that the database is accessible from the server.
