---
comments: true
---

import LastCheckedWithVersionsNotice from "@site/src/components/LastCheckedWithVersionsNotice";
import AddExternalAuthEnvVarsReminder from './_addExternalAuthEnvVarsReminder.md'
import { Server, Client, Database } from '../DeploymentTag'

# Render

<LastCheckedWithVersionsNotice versions={{ Wasp: "0.26", Render: new Date("2026-04-15") }} />

## Deploy Wasp on Render <Server /> <Client /> <Database />

This guide shows you how to deploy your app and provision a database on Render.

A built Wasp app is a single server that serves your app's pages, its static assets and its API, so it needs a single Render Web Service.

Unlike the other providers listed here, Render builds your Wasp app from source on its servers, so you don't need to run `wasp build` locally before deploying. You'll define your entire deployment setup in a `render.yaml` file that Render uses as a [Blueprint](https://docs.render.com/infrastructure-as-code) to create and configure all services.

### Prerequisites

To get started, follow these steps:

1. Create a [Render](https://render.com/) account.
1. Push your Wasp project to a Git repository (GitHub, GitLab, or Bitbucket).
1. Generate your initial database migrations locally by running `wasp db migrate-dev` and commit the `migrations/` directory. Render needs these migration files in the repo to set up your database.

### Create the render.yaml Blueprint

Create a `render.yaml` file in the root of your repository. This defines both services (the app and the database):

```yaml title="render.yaml"
services:
  # The Wasp app -- Render installs Wasp and builds from source
  - type: web
    name: <app-name>
    runtime: node
    plan: <plan>
    region: <region>
    branch: main
    buildCommand: >-
      npm install -g @wasp.sh/wasp-cli@<wasp-version> &&
      export PATH="$(npm prefix -g)/bin:$PATH" &&
      wasp install &&
      wasp build &&
      npm install &&
      cd .wasp/out/server &&
      npm install &&
      npx prisma generate --schema=../db/schema.prisma &&
      cd ../../.. &&
      npx vite build
    startCommand: cd .wasp/out/server && npm run start-production
    envVars:
      - key: DATABASE_URL
        fromDatabase:
          name: <app-name>-db
          property: connectionString
      - key: JWT_SECRET
        generateValue: true
      - key: WASP_SERVER_URL
        sync: false # you'll fill this in after the first deploy
      - key: NODE_VERSION
        value: "24"

databases:
  - name: <app-name>-db
    plan: <plan>
    region: <region>
    postgresMajorVersion: "18"
```

You should replace the following values for your app:

| Variable | Value | Example |
|---|---|---|
| `<app-name>` | A unique name for your app | `my-wasp-app` |
| `<wasp-version>` | The Wasp CLI version you're using | `0.26` |
| `<plan>` | The Render plan for your services | `free` |
| `<region>` | The Render region closest to your users | `oregon` |

:::caution
The Render free-tier PostgreSQL database [expires after 30 days](https://render.com/docs/free#30-day-limit). Use the Starter plan or an external provider for production.
:::

Commit this file and push to your repository:

```bash
git add render.yaml
git commit -m "Add Render Blueprint"
git push origin main
```

### Deploy with the Blueprint

1. In the Render Dashboard, click **New > Blueprint**.
2. Connect your Git repository and select the branch with the `render.yaml`.
3. Render will parse the Blueprint and show the resources it will create. Do not fill out the environment variables form yet. Click **Apply**.

This will try to create both services. It will fail initially, as some environment variables are missing.

#### Set the Environment Variables

Wait until both services are created. Go to the Web Service in the Render Dashboard and note its URL (usually `https://<app-name>.onrender.com`).

Go to **Settings > Environment** and set the following variable. When you're done, click **Save and rebuild**:

| Variable | Value |
|---|---|
| `WASP_SERVER_URL` | `https://<app-name>.onrender.com` |

:::note
There's no separate client URL to configure. `WASP_WEB_CLIENT_URL` defaults to `WASP_SERVER_URL`, and one server serves both your app's pages and its API.
:::

<AddExternalAuthEnvVarsReminder />

:::caution Client env variables
Render builds your app on its servers, so the Web Service's env variables are the ones the build sees. Set any [client env variables](../../../deployment/env-vars.md#client-env-vars) your app needs (the ones prefixed with `REACT_APP_`) on the Web Service as well.

Their values are written into your app's pages as they are built, so they have to be there **before** the build runs, and changing one takes a new deploy to take effect.
:::

### Redeploying After Changes

Render auto-deploys when it detects a new commit on the configured branch. Just push your changes:

```bash
git push origin main
```

If you have new database model changes, make sure to run `wasp db migrate-dev` locally first and commit the generated migration files along with your code changes. The server runs `prisma migrate deploy` on startup, so new migrations are applied automatically on each deploy.

:::note Build time
The service installs Wasp and compiles the app from source on each deploy. On the free tier, this can take 10-15 minutes. If builds consistently time out, consider upgrading to the Starter plan.
:::
