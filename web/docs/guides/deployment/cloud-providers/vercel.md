---
comments: true
---

import LastCheckedWithVersionsNotice from "@site/src/components/LastCheckedWithVersionsNotice";
import AddExternalAuthEnvVarsReminder from './_addExternalAuthEnvVarsReminder.md'
import BuildingTheWebClient from '../../../deployment/deployment-methods/_building-the-web-client.md'
import { SecretGeneratorBlock } from '../../../project/SecretGeneratorBlock'
import { Server, Client, Database } from '../DeploymentTag'

# Vercel

<LastCheckedWithVersionsNotice versions={{ Wasp: "0.26", Vercel: new Date("2026-09-07") }} />

## Deploy Wasp to Vercel <Server /> <Client /> <Database />

This guide shows you how to deploy the server as an Express app running on [Vercel Functions](https://vercel.com/docs/functions), deploy the client as a static site, and provision a PostgreSQL database from the [Vercel Marketplace](https://vercel.com/marketplace). You will need a Vercel account to follow these instructions.

### Overview

A Wasp app has three parts. While they all come from the same project, on Vercel each one gets its own home:

- **The server** <Server /> becomes a [Vercel Function](https://vercel.com/docs/functions) in its own project. Vercel builds it and wraps the generated Express app in a single function using its [Express preset](https://vercel.com/docs/frameworks/backend/express).
- **The client** <Client /> is deployed as a static site to a second Vercel project. You build it locally with Vite and upload the resulting assets, with an extra rule to make sure non-[prerendered](../../../advanced/prerendering.md) pages work correctly.
- **The database** <Database /> is a PostgreSQL database, provisioned through the Vercel Marketplace (we use [Supabase](https://supabase.com/) in this guide). Vercel injects the connection details into your server project as environment variables.

Throughout this guide, we will use `my-wasp-app-server` and `my-wasp-app-client` as the names of the Vercel projects for the server and client, respectively, but you can pick any names you like. Vercel gives each project a production domain of the form `https://<project-name>.vercel.app` (you can change it to a custom domain in your **Settings → Domains** tab).

### Limitations

Your Wasp server runs as a [Vercel Function](https://vercel.com/docs/functions), which is a serverless platform. This works well for most apps, but it does come with some constraints you should be aware of:

- Functions spin up on demand and run for up to 5 minutes by default. If your app needs to run long-running processes, Vercel Functions are not a good fit:
  - [Jobs](../../../advanced/jobs.md) are not supported.
  - [WebSockets](../../../advanced/web-sockets.md) are not supported.
  - Global state (e.g. singletons or top-level variables) will be reset periodically, so you should only use it for caching and not for storing important data or app logic.
- If your app defines a [server `setupFn`](../../../project/server-config.md#setup-function), it must finish quickly (well under a second). Vercel waits a limited time for the server to start listening before it gives up on a request.

:::caution
As Wasp does not yet have a way to detect where you're deploying your server to, we can't warn you if your app relies on any of the above features, so please double check before deploying to Vercel.
:::

If your app relies on any of these, deploy the server to a different provider (see the [other guides](../../../deployment/deployment-methods/cloud-providers.md)). You may still use Vercel to deploy your Wasp app's client.

### Pricing

We estimate that Vercel's free plan is sufficient as a testing ground for most apps, or as production deployment for small-scale, non-commercial apps. For a large-scale app or a commercial one, you may need to consider a paid plan to handle increased traffic.

Please check Vercel's [pricing page](https://vercel.com/pricing#:~:text=Vercel%20Functions) for the most up-to-date information on their plans and their allowances; as well as their [Fair Usage guidelines](https://vercel.com/docs/limits/fair-use-guidelines#commercial-usage) on what constitutes commercial use.

### Prerequisites

To get started, follow these steps:

1. Make sure your Wasp app is built by running `wasp build` in the project dir.
1. Create a [Vercel](https://vercel.com/signup) account.
1. Log in with the Vercel CLI by running `npx vercel login`. A browser tab will open to authenticate you.
1. Go to your Wasp project root and run `npx vercel link`. Follow the prompts to link your project to a new Vercel project for your Wasp app's server (e.g. `my-wasp-app-server`), and use the default settings otherwise.

### Setting up the database

We'll use [Supabase](https://supabase.com/) from the Vercel Marketplace. From your project root (which should now be linked to the server project), run:

```shell
npx vercel integration add supabase
```

The first time, the CLI will ask you to accept the Marketplace terms in your browser. Open the link it prints, accept the terms and run the command again. Vercel creates a Supabase database, connects it to your server project and adds a set of `POSTGRES_*` and `SUPABASE_*` environment variables to it.

Wasp expects the connection string in the `DATABASE_URL` environment variable, so we need to copy it over. Use the value of `POSTGRES_PRISMA_URL`: this is Supabase's transaction-mode pooler, which is the one [recommended for serverless](https://supabase.com/docs/guides/database/connecting-to-postgres#pooler-transaction-mode). You can use the CLI to copy the value from one environment variable to the other:

```shell
npx vercel env run -- printenv POSTGRES_PRISMA_URL
# Copy the line that starts with "postgres://"

npx vercel env add --no-sensitive DATABASE_URL production
# Paste it when prompted
```

Prisma migrations can't run through the transaction-mode pooler, so the build command in the next section runs them against `POSTGRES_URL_NON_POOLING` (Supabase's session-mode pooler) instead.

:::warning
The Supabase database is linked to your Vercel account. If you remove the integration or delete your Vercel account, the database will be deleted along with it.
:::

You can also use the Supabase dashboard to manage your database, run queries, and view logs. You can access it from the **Storage** tab of your Vercel project: click the database, then the **Open in Supabase** button.

### Deploying the server

1. Create a `vercel.json` in your project root that tells Vercel to treat the built server as an Express app:

   ```json title="vercel.json"
   {
     "$schema": "https://openapi.vercel.sh/vercel.json",
     "framework": "express",
     "installCommand": "npm install",
     "buildCommand": "cd .wasp/out/server && npm install && npx prisma generate --schema=../db/schema.prisma && npm run bundle && DATABASE_URL=$POSTGRES_URL_NON_POOLING npm run db-migrate-prod",
     "outputDirectory": ".wasp/out/server/bundle"
   }
   ```

   The build command mirrors what the generated `Dockerfile` does: it installs the server's dependencies, generates the Prisma client, bundles the server and runs any pending database migrations (using the session-mode pooler, as explained above). Vercel then looks for the bundled `server.js` in the output directory and wraps it in a function.

1. Create a `.vercelignore` that keeps local files (and, importantly, your `.env` files) out of the upload:

   ```gitignore title=".vercelignore"
   node_modules
   .vercel
   .env*
   /public
   /migrations
   .wasp/out/web-app
   .wasp/out/server/bundle
   .wasp/out/user
   ```

   <small>
     `/migrations` is excluded because `wasp build` already copies your migrations into `.wasp/out/db/`, which is where the build command runs them from. `/public` is excluded so Vercel doesn't serve your client's static files from the server's domain.
   </small>

1. Add the remaining [required server env variables](../../../project/env-vars.md#server-general-configuration) to the production environment, one at a time, with `npx vercel env add <NAME> production`:
   - `PORT` set to `3000`.
   - `WASP_WEB_CLIENT_URL` set to the client's domain (e.g. `https://my-wasp-app-client.vercel.app`). `https://` prefix is required!
   - `WASP_SERVER_URL` set to the server's domain (e.g. `https://my-wasp-app-server.vercel.app`). `https://` prefix is required!
   - `JWT_SECRET` set to a random string at least 32 characters long<br /><SecretGeneratorBlock />

   <AddExternalAuthEnvVarsReminder />

1. Deploy the server from your project root:

   ```shell
   npx vercel deploy --prod
   ```

   Vercel uploads the project, runs the build command (including the database migrations) and turns the bundled server into a function. Once it finishes, your server is live at `https://my-wasp-app-server.vercel.app`.

### Deploying the client

The client is deployed as a static site from the `.wasp/out/web-app/build` directory. Wasp uses a `200.html` file as a fallback for client-side pages, so we'll need to tell Vercel about it:

1. Create a `vercel.json` file in your project's `public/` directory with the following content:

   ```json title="public/vercel.json"
   {
     "$schema": "https://openapi.vercel.sh/vercel.json",
     "rewrites": [{ "source": "/(.*)", "destination": "/200.html" }]
   }
   ```

   When a user visits your app, Vercel tries to find a prerendered page by default. If it can't find it, it will return the `200.html` file that renders client-side pages.

1. <BuildingTheWebClient />

1. Next, create the Vercel project for the client:

   ```shell
   npx vercel project add my-wasp-app-client
   ```

1. Finally, deploy the build directory to the client project:

   ```shell
   cd .wasp/out/web-app/build
   npx vercel deploy --prod --yes --project my-wasp-app-client
   ```

That is it! Your app should be live at `https://my-wasp-app-client.vercel.app`.

### Updates and redeploying

When you make updates and need to redeploy:

1. Run `wasp build` to rebuild your app.
1. Deploy the server from the project root:

   ```shell
   npx vercel deploy --prod
   ```

1. Rebuild the client from the project root:

   ```shell
   REACT_APP_API_URL=<url_to_wasp_backend> npx vite build
   ```

   And then deploy the client with:

   ```shell
   cd .wasp/out/web-app/build
   npx vercel deploy --prod --yes --project my-wasp-app-client
   ```
