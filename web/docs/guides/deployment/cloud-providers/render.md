---
comments: true
last_checked_with_versions:
  Wasp: "0.23"
  Render: 2026-04-15
---

import AddExternalAuthEnvVarsReminder from './_addExternalAuthEnvVarsReminder.md'
import { SecretGeneratorBlock } from '../../../project/SecretGeneratorBlock'
import { Server, Client, Database } from '../DeploymentTag'

# Render

## Deploy Wasp to Render <Server /> <Client /> <Database />

This guide shows you how to deploy the server, client, and provision a database on Render.

Unlike the other providers listed here, Render builds your Wasp app from source on its servers, so you don't need to run `wasp build` locally before deploying. You'll define your entire deployment setup in a `render.yaml` file that Render uses as a [Blueprint](https://docs.render.com/infrastructure-as-code) to create and configure all services.

### Prerequisites

To get started, follow these steps:

1. Create a [Render](https://render.com/) account.
1. Push your Wasp project to a Git repository (GitHub or GitLab).
1. Generate your initial database migrations locally by running `wasp db migrate-dev` and commit the `migrations/` directory. Render needs these migration files in the repo to set up your database.

### Create the render.yaml Blueprint

Create a `render.yaml` file in the root of your repository. This defines all three services (database, server, and client):

```yaml
services:
  # Node.js server -- Render installs Wasp and builds from source
  - type: web
    name: <app-name>-server
    runtime: node
    plan: free
    region: oregon # pick the region closest to your users
    branch: main
    buildCommand: >-
      npm install -g @wasp.sh/wasp-cli &&
      export PATH="$(npm prefix -g)/bin:$PATH" &&
      wasp build &&
      cd .wasp/out/server &&
      npm install &&
      npx prisma generate --schema=../db/schema.prisma &&
      npm run bundle
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
      - key: WASP_WEB_CLIENT_URL
        sync: false # you'll fill this in after the first deploy

  # React client -- static site built with Vite
  - type: web
    name: <app-name>-client
    runtime: static
    branch: main
    buildCommand: >-
      npm install -g @wasp.sh/wasp-cli &&
      export PATH="$(npm prefix -g)/bin:$PATH" &&
      wasp build &&
      npx vite build
    staticPublishPath: .wasp/out/web-app/build
    envVars:
      - key: REACT_APP_API_URL
        sync: false # you'll fill this in after the first deploy
    routes:
      - type: rewrite
        source: /*
        destination: /200.html

databases:
  - name: <app-name>-db
    plan: free
    region: oregon # must match the server region
    postgresMajorVersion: "18"
```

:::caution
The Render free-tier PostgreSQL database expires after 90 days. Use the Starter plan or an external provider for production.
:::

Commit this file and push to your repository:

```bash
git add render.yaml
git commit -m "Add Render Blueprint"
git push origin main
```

### Deploy with the Blueprint

1. In the Render Dashboard, click **New > Blueprint**
2. Connect your Git repository and select the branch with the `render.yaml`
3. Render will parse the Blueprint and show the resources it will create. Review them and click **Apply**

This creates all three services and the database at once. Once they're provisioned, you need to set a few environment variables that couldn't be determined ahead of time.

#### Set the Environment Variables

Go to each service in the Render Dashboard and note its URL (e.g., `https://<app-name>-server.onrender.com` and `https://<app-name>-client.onrender.com`).

On the **server** Web Service, go to **Settings > Environment** and set:

| Variable | Value |
|---|---|
| `WASP_SERVER_URL` | `https://<app-name>-server.onrender.com` |
| `WASP_WEB_CLIENT_URL` | `https://<app-name>-client.onrender.com` |

We can help you generate a `JWT_SECRET` if you didn't use the `generateValue: true` option in the Blueprint:<br/><SecretGeneratorBlock />

<AddExternalAuthEnvVarsReminder />

On the **client** Static Site, go to **Settings > Environment** and set:

| Variable | Value |
|---|---|
| `REACT_APP_API_URL` | `https://<app-name>-server.onrender.com` |

:::caution
`REACT_APP_API_URL` must be set **before** the client build runs. Vite embeds it into the compiled JavaScript at build time. If it's missing, all API calls from the client will fail.
:::

After setting all the variables, trigger a manual redeploy for both services so they pick up the new values.

:::tip Using a Render Environment Group
Rather than setting variables on each service separately, you can create an [Environment Group](https://docs.render.com/configure-environment-variables#environment-groups) and link it to both services to manage shared variables in one place.
:::

### Redeploying After Changes

Render auto-deploys when it detects a new commit on the configured branch. Just push your changes:

```bash
git push origin main
```

If you have new database model changes, make sure to run `wasp db migrate-dev` locally first and commit the generated migration files along with your code changes. The server runs `prisma migrate deploy` on startup, so new migrations are applied automatically on each deploy.

:::note Build time
Both services install Wasp and compile the app from source on each deploy. On the free tier, this can take 10-15 minutes. If builds consistently time out, consider upgrading to the Starter plan.
:::
