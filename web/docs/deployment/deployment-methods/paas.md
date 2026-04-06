---
title: Cloud Providers
---

import BuildingTheWebClient from '../\_building-the-web-client.md'
import { Server, Client, Database } from './DeploymentTag'
import { GuideLink } from '@site/src/components/GuideLink'

You can deploy the built Wasp app wherever and however you want, as long as your provider/server supports Wasp's build format.

After going through the general steps that apply to all deployments, you can
follow step-by-step guides for deploying your Wasp app to some of the most popular
providers:

- [Fly.io](#flyio)
- [Railway](#railway)
- [Heroku](#heroku)
- [Netlify](#netlify)
- [Cloudflare](#cloudflare)

No worries, you can still deploy your app if your desired provider isn't on the
list - it just means we don't yet have a step-by-step guide for you to follow.
Feel free to [open a
PR](https://github.com/wasp-lang/wasp/edit/release/web/docs/deployment/deployment-methods/paas.md)
if you'd like to write one yourself :)

## Deploying a Wasp App

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

### 2. Deploying the API Server <Server />

There's a Dockerfile that defines an image for building the server in the `.wasp/out` directory.

To run the server in production, deploy this Docker image to a hosting provider and make sure the required env variables are correctly set up. Usually, you use the provider's dashboard UI or a CLI tool to set up these env variables.

Check the [required server env variables](../env-vars.md#server-env-vars) and make sure they are set up for your server.

While these are the general instructions on deploying the server anywhere, we also have more detailed instructions for chosen providers below, so check that out for more guidance if you are deploying to one of those providers.

### 3. Deploying the Web Client <Client />

<BuildingTheWebClient />

The command above will build the web client and put it in the `.wasp/out/web-app/build` directory, including the `200.html` file at the root that acts as the SPA fallback.

Since the result of building is just a bunch of static files, you can now deploy your web client to any static hosting provider (e.g. Netlify, Cloudflare, ...) by deploying the contents of `.wasp/out/web-app/build/`.

### 4. Deploying the Database <Database />

<!-- TOPIC: database -->

Any PostgreSQL database will do, as long as you provide the server with the correct `DATABASE_URL` env var and ensure that the database is accessible from the server.

## Different Providers

We'll cover a few different deployment providers below:

- Fly.io <Server /> <Database />
- Railway <Server /> <Client /> <Database />
- Heroku <Server /> <Database />
- Netlify <Client />
- Cloudflare <Client />

## Fly.io <Server /> <Database /> {#flyio}

Deploy your server and provision a database on Fly.io.

#### Overview of the steps

1. Create a [Fly.io](https://fly.io/) account and install the [`fly` CLI](https://fly.io/docs/flyctl/install/).
1. [Build your app](#1-generating-deployable-code) with `wasp build`.
1. Set up a new Fly.io app with `fly launch --remote-only` in `.wasp/out/`.
1. Configure the required [env variables](../env-vars.md) with `fly secrets set`.
1. Deploy the server with `fly deploy --remote-only`.
1. Deploy the client to a static hosting provider (e.g. [Netlify](#netlify) or [Cloudflare](#cloudflare)).

<GuideLink linkToGuide="../../guides/deployment/flyio" title="Deploying Wasp to Fly.io" description="Uses Fly.io, fly CLI, Docker" />

## Railway <Server /> <Client /> <Database /> {#railway}

Deploy the client, the server, and provision a database on Railway.

#### Overview of the steps

1. Create a [Railway](https://railway.com/?utm_medium=integration&utm_source=docs&utm_campaign=wasp) account and install the [Railway CLI](https://docs.railway.com/develop/cli?utm_medium=integration&utm_source=docs&utm_campaign=wasp#installing-the-cli).
1. [Build your app](#1-generating-deployable-code) with `wasp build`.
1. Create a Railway project with PostgreSQL, server, and client services.
1. Configure the required [env variables](../env-vars.md) in the Railway dashboard.
1. Deploy the server and client with `railway up`.

<GuideLink linkToGuide="../../guides/deployment/railway" title="Deploying Wasp to Railway" description="Uses Railway, Railway CLI" />


## Heroku <Server /> <Database /> {#heroku}

Deploy the server and provision a database on Heroku.

#### Overview of the steps

1. Create a [Heroku](https://www.heroku.com/) account and install the [`heroku` CLI](https://devcenter.heroku.com/articles/heroku-cli) and `docker` CLI.
1. [Build your app](#1-generating-deployable-code) with `wasp build`.
1. Create a Heroku app and provision a PostgreSQL database.
1. Configure the required [env variables](../env-vars.md) with `heroku config:set`.
1. Build and push the Docker image, then release it.
1. Deploy the client to a static hosting provider (e.g. [Netlify](#netlify) or [Cloudflare](#cloudflare)).

<GuideLink linkToGuide="../../guides/deployment/heroku" title="Deploying Wasp to Heroku" description="Uses Heroku, heroku CLI, Docker" />

## Netlify <Client /> {#netlify}

Deploy your client to Netlify, a free static hosting provider.

#### Overview of the steps

1. Create a [Netlify](https://www.netlify.com/) account.
1. [Build your app](#1-generating-deployable-code) with `wasp build`.
1. [Build the client](#3-deploying-the-web-client-) with `npx vite build`.
1. Create a `netlify.toml` config file for SPA routing.
1. Deploy with `npx netlify-cli deploy --prod`.

<GuideLink linkToGuide="../../guides/deployment/netlify" title="Deploying Wasp to Netlify" description="Uses Netlify, Netlify CLI" />

## Cloudflare <Client /> {#cloudflare}

Deploy your client to Cloudflare Workers, a free hosting service.

#### Overview of the steps

1. Create a [Cloudflare](https://www.cloudflare.com/) account.
1. [Build your app](#1-generating-deployable-code) with `wasp build`.
1. [Build the client](#3-deploying-the-web-client-) with `npx vite build`.
1. Deploy with `npx wrangler pages deploy`.

<GuideLink linkToGuide="../../guides/deployment/cloudflare" title="Deploying Wasp to Cloudflare Workers" description="Uses Cloudflare Workers, Wrangler CLI" />
