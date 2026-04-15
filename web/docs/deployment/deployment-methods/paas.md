---
title: Cloud Providers
---

import BuildingTheWebClient from './_building-the-web-client.md'
import { GuideLink } from '@site/src/components/GuideLink'

You can deploy the built Wasp app wherever and however you want, as long as your provider/server supports running a Node.js server, serving static files, and running a PostgreSQL database.

## Guides

We have step-by-step guides for deploying your Wasp app to some of the most popular providers you can follow:

<GuideLink linkToGuide="../../guides/deployment/flyio" title="Deploying Wasp to Fly.io" description="Uses Fly.io, fly CLI, Docker" />

<GuideLink linkToGuide="../../guides/deployment/railway" title="Deploying Wasp to Railway" description="Uses Railway, Railway CLI" />

<GuideLink linkToGuide="../../guides/deployment/heroku" title="Deploying Wasp to Heroku" description="Uses Heroku, heroku CLI, Docker" />

<GuideLink linkToGuide="../../guides/deployment/netlify" title="Deploying Wasp to Netlify" description="Uses Netlify, Netlify CLI" />

<GuideLink linkToGuide="../../guides/deployment/cloudflare" title="Deploying Wasp to Cloudflare Workers" description="Uses Cloudflare Workers, Wrangler CLI" />

If your desired provider isn't on the list, no worries, you can still deploy your app  - it just means we don't yet have a step-by-step guide for you to follow.
Feel free to [open a PR](https://github.com/wasp-lang/wasp/edit/release/web/docs/deployment/deployment-methods/paas.md) if you'd like to write one yourself :)

## Manual deployment

Deploying a Wasp app comes down to the following:

1. Generating deployable code.
2. Deploying the API server (backend).
3. Deploying the web client (frontend).
4. Deploying a PostgreSQL database and keeping it running.

Let's go through each of these steps.

### 1. Generating Deployable Code

Running the command `wasp build` generates deployable code for the whole app in the `.wasp/out/` directory.

```
wasp build
```

:::caution PostgreSQL in production
You won't be able to build the app if you are using SQLite as a database (which is the default database).
You'll have to [switch to PostgreSQL](../../data-model/databases.md#migrating-from-sqlite-to-postgresql) before deploying to production.
:::

### 2. Deploying the API Server

There's a Dockerfile that defines an image for building the server in the `.wasp/out` directory.

To run the server in production, deploy this Docker image to a hosting provider and make sure the required env variables are correctly set up. Usually, you use the provider's dashboard UI or a CLI tool to set up these env variables.

Check the [required server env variables](../env-vars.md#server-env-vars) and make sure they are set up for your server.

While these are the general instructions on deploying the server anywhere, we also have more detailed instructions for chosen providers below, so check that out for more guidance if you are deploying to one of those providers.

### 3. Deploying the Web Client

<BuildingTheWebClient />

The command above will build the web client and put it in the `.wasp/out/web-app/build` directory, including the `200.html` file at the root that acts as the SPA fallback.

Since the result of building is just a bunch of static files, you can now deploy your web client to any static hosting provider (e.g. Netlify, Cloudflare, ...) by deploying the contents of `.wasp/out/web-app/build/`.

### 4. Deploying the Database

Any PostgreSQL database will do, as long as you provide the server with the correct `DATABASE_URL` env var and ensure that the database is accessible from the server.
