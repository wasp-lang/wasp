---
comments: true
---

import LastCheckedWithVersionsNotice from "@site/src/components/LastCheckedWithVersionsNotice";
import AddExternalAuthEnvVarsReminder from './_addExternalAuthEnvVarsReminder.md'
import { SecretGeneratorBlock } from '../../../project/SecretGeneratorBlock'
import { Server, Client, Database } from '../DeploymentTag'

# Zerops

<LastCheckedWithVersionsNotice versions={{ Wasp: "0.25", Zerops: new Date("2026-09-08") }} />

## Deploy Wasp on Zerops <Server /> <Client /> <Database />

This guide shows you how to deploy the server, the client, and provision a database on [Zerops](https://zerops.io/).

Zerops builds your Wasp app from source on its servers, so you don't need to run `wasp build` locally before deploying. You'll define the project topology in an `import.yaml` and the build/deploy pipeline in a `zerops.yaml`. Wasp maps to three Zerops services: a static client (`app`), a Node.js API (`api`), and PostgreSQL (`db`).

### Prerequisites

To get started, follow these steps:

1. Create a [Zerops](https://zerops.io/) account.
1. Push your Wasp project to a Git repository.
1. Generate your initial database migrations locally by running `wasp db migrate-dev` and commit the `migrations/` directory. Zerops needs these migration files in the repo to set up your database.

### Create the zerops.yaml

Create a `zerops.yaml` in the root of your repository:

```yaml title="zerops.yaml"
# yaml-language-server: $schema=https://api.app-prg1.zerops.io/api/rest/public/settings/zerops-yaml-json-schema.json

zerops:
  # React client — Wasp + Vite build, then Nginx serves the static files
  - setup: prod-client
    build:
      base: nodejs@24
      buildCommands:
        - npm install -g @wasp.sh/wasp-cli@<wasp-version>
        - export PATH="$(npm prefix -g)/bin:$PATH"
        - wasp install
        - wasp build
        - npx vite build
      envVariables:
        # Vite embeds this at build time — must be the public API URL
        REACT_APP_API_URL: ${API_URL}
      deployFiles:
        # ~ strips the path prefix so index.html is the Nginx docroot
        - .wasp/out/web-app/build/~
      cache:
        - node_modules
    run:
      base: static

  # Node.js API — Wasp build, Prisma generate, bundle, then run the server
  - setup: prod-api
    build:
      base: nodejs@24
      os: ubuntu
      buildCommands:
        - npm install -g @wasp.sh/wasp-cli@<wasp-version>
        - export PATH="$(npm prefix -g)/bin:$PATH"
        - wasp install
        - wasp build
        - cd .wasp/out/server && npm install
        - cd .wasp/out/server && npx prisma generate --schema=../db/schema.prisma
        - cd .wasp/out/server && npm run bundle
      deployFiles:
        # Keep the .wasp/out tree — runtime commands cd into .wasp/out/server
        - .wasp/out
      cache:
        - node_modules
    deploy:
      readinessCheck:
        httpGet:
          port: 3001
          path: /auth/me
    run:
      base: nodejs@24
      os: ubuntu
      initCommands:
        # Runs once per new app version (key includes ${appVersionId})
        - zsc execOnce ${appVersionId}-migrate --retryUntilSuccessful -- sh -c 'cd .wasp/out/server && npx prisma migrate deploy --schema=../db/schema.prisma'
      ports:
        - port: 3001
          httpSupport: true
      envVariables:
        NODE_ENV: production
        PORT: 3001
        DATABASE_URL: postgresql://${db_user}:${db_password}@${db_hostname}:${db_port}/${db_dbName}
        JWT_SECRET: ${APP_SECRET}
        WASP_SERVER_URL: ${API_URL}
        WASP_WEB_CLIENT_URL: ${APP_URL}
      start: sh -c 'cd .wasp/out/server && NODE_ENV=production node --enable-source-maps bundle/server.js'
```

Replace `<wasp-version>` with the Wasp CLI version your app uses (for example `0.25`).

The `prod-client` setup runs `wasp install && wasp build`, then `npx vite build`. `REACT_APP_API_URL` must be set at **build time** — Vite embeds it into the compiled JavaScript. If it's missing, all API calls from the client will fail.

The `prod-api` setup bundles the server, then on each new deploy version `zsc execOnce` runs `prisma migrate deploy` once before the server starts on port **3001**. Runtime env vars map from the project value store (set in `import.yaml` below):

| Variable | Value | Notes |
| --- | --- | --- |
| `DATABASE_URL` | `postgresql://${db_user}:…` | Zerops injects `db` service credentials automatically |
| `WASP_SERVER_URL` | `${API_URL}` | Public API URL, including `https://` |
| `WASP_WEB_CLIENT_URL` | `${APP_URL}` | Public client URL, including `https://` |
| `JWT_SECRET` | `${APP_SECRET}` | At least 32 characters — set `APP_SECRET` in `import.yaml` |
| `PORT` | `3001` | Must match `API_URL` and the readiness check |

Do not self-reference Wasp keys in `zerops.yaml` (for example `${WASP_SERVER_URL}`). Only value-store keys (`APP_URL`, `API_URL`, `APP_SECRET`) and Zerops service keys (`${db_password}`, …) resolve.

### Create the import.yaml

`import.yaml` defines which services exist and the project **value store** — generic `APP_URL`, `API_URL`, and `APP_SECRET` that `zerops.yaml` maps into Wasp's env var names. Do not put `envVariables` on service blocks in `import.yaml`.

```yaml title="import.yaml"
#yamlPreprocessor=on
# yaml-language-server: $schema=https://api.app-prg1.zerops.io/api/rest/public/settings/import-project-yaml-json-schema.json

project:
  name: my-wasp-app-small-prod
  envVariables:
    APP_URL: https://app-${zeropsSubdomainHost}.prg1.zerops.app
    API_URL: https://api-${zeropsSubdomainHost}-3001.prg1.zerops.app
  envSecrets:
    # Generated once at import — requires #yamlPreprocessor=on as the first line
    APP_SECRET: <@generateRandomString(<64>)>

services:
  - hostname: app
    type: static
    priority: 5
    zeropsSetup: prod-client
    buildFromGit: https://github.com/<your-org>/<your-wasp-app>
    enableSubdomainAccess: true
    minContainers: 2

  - hostname: api
    type: nodejs@24
    priority: 5
    zeropsSetup: prod-api
    buildFromGit: https://github.com/<your-org>/<your-wasp-app>
    enableSubdomainAccess: true
    minContainers: 2

  - hostname: db
    type: postgresql:single@16
    profile: oltp-hobby
    priority: 10
```

Replace the following values for your app:

| Variable | Value | Example |
| --- | --- | --- |
| `<your-org>/<your-wasp-app>` | Your GitHub repository | `my-org/my-wasp-app` |
| `project.name` | A unique name for this Zerops project | `my-wasp-app-small-prod` |

The `zeropsSubdomainHost` placeholder is filled in by Zerops at import time. `zeropsSetup` on each service must match a `setup:` name in `zerops.yaml`.

Because `zerops.yaml` maps `JWT_SECRET` to `${APP_SECRET}`, you must supply `APP_SECRET` in the project value store. Setting `JWT_SECRET` in the dashboard will not work — runtime `envVariables` in `zerops.yaml` take precedence over dashboard secrets. Generate `APP_SECRET` at import with the preprocessor above, or add it manually in the dashboard (**Project → Environment variables → Secrets**) as a random string at least 32 characters long:<br /><SecretGeneratorBlock />

<AddExternalAuthEnvVarsReminder />

This example uses the **Small Production** topology (two containers each). For a single-container stage setup, omit `minContainers` on the `app` and `api` services.

The [official recipe's Small Production `import.yaml`](https://github.com/zeropsio/recipes/blob/main/wasp-hello-world/4%20%E2%80%94%20Small%20Production/import.yaml) uses the same service topology but points at the demo app repo and does not define `APP_SECRET` — the demo hardcodes `JWT_SECRET` in its `zerops.yaml` instead.

### Deploy

1. Commit `zerops.yaml` and push your repository.
1. In the Zerops dashboard, create a project and **Import** the `import.yaml`, or run `zcli project project-import`.
1. Point `buildFromGit` at your repository (or connect the GitHub / GitLab integration so later pushes rebuild automatically).
1. Wait for `db`, then `api`, then `app` to finish building.

### Verify your deployment

- **Client:** open the `app` URL — the SPA loads.
- **API:** `GET /auth/me` on port 3001 responds (this is the readiness check).
- **Auth:** if login fails, check that `WASP_WEB_CLIENT_URL` and `WASP_SERVER_URL` match the public client and API URLs in the project value store.

### Redeploying After Changes

When the `app` and `api` services are connected to your Git repository, push to the linked branch:

```bash
git push origin main
```

Zerops starts a new build of each connected service. You can also trigger a rebuild from the dashboard or with `zcli service trigger-build`.

If you changed the Prisma schema, run `wasp db migrate-dev` locally first and commit the generated files in `migrations/` along with your code. On the next API deploy, `zsc execOnce` runs `prisma migrate deploy` once for that new app version before the server starts.

Changing `APP_URL` or `API_URL` in the project value store requires a **rebuild** of the client and a **restart** of the API.

### Official recipe (optional)

Zerops maintains an official [Wasp Hello World recipe](https://app.zerops.io/recipes/wasp-hello-world) that imports a ready-made project with one click. It covers several environments — from AI-assisted development to production:

| Environment | Best for |
| --- | --- |
| [**AI Agent**](https://app.zerops.io/recipes/wasp-hello-world?environment=ai-agent) | Coding agents — SSH into dev containers, run `wasp start` |
| [**Remote (CDE)**](https://app.zerops.io/recipes/wasp-hello-world?environment=remote-cde) | Cloud dev environment over SSH |
| [**Local**](https://app.zerops.io/recipes/wasp-hello-world?environment=local) | Laptop + cloud DB via [zCLI VPN](#local-development) |
| [**Stage**](https://app.zerops.io/recipes/wasp-hello-world?environment=stage) | Pre-production / QA |
| [**Small Production**](https://app.zerops.io/recipes/wasp-hello-world?environment=small-production) | Small live apps |
| [**Highly-available Production**](https://app.zerops.io/recipes/wasp-hello-world?environment=highly-available-production) | Production with HA PostgreSQL |

Recipe `import.yaml` files live in the [recipe repo](https://github.com/zeropsio/recipes/tree/main/wasp-hello-world); the [demo app](https://github.com/zerops-recipe-apps/wasp-hello-world-app) provides the reference `zerops.yaml`. AI Agent and Remote environments use `appstage` as the preview client (not `app`) and add `appdev` / `apidev` containers for live development.

If you start from the recipe and later switch `buildFromGit` to your own repo, add `APP_SECRET` to the project value store first — the recipe `import.yaml` does not include it.

### Local development

The recipe's [**Local**](https://app.zerops.io/recipes/wasp-hello-world?environment=local) environment deploys client, API, and PostgreSQL on Zerops while you run `wasp start` on your machine.

`zcli vpn up` only gives you network access to project hostnames (so `db:5432` resolves). Zerops does **not** inject environment variables over the VPN — you must pass `DATABASE_URL` yourself.

1. Install [zCLI](https://docs.zerops.io/references/cli) if you haven't already.
1. In the Zerops dashboard, open the `db` service and copy the PostgreSQL user, password, port, and database name from **Environment variables**.
1. Connect to the project network, then start Wasp with an explicit URL (do not put production credentials in `.env.server`):

```bash
zcli vpn up
DATABASE_URL="postgresql://<user>:<password>@db:<port>/<database>" wasp start
```

Use the hostname `db` (the service hostname), not a public host — that only works while the VPN is up. If hostname resolution fails, try `db.zerops` as described in the [Zerops VPN docs](https://docs.zerops.io/references/networking/vpn).

### Resources

- [Wasp Hello World recipe](https://app.zerops.io/recipes/wasp-hello-world)
- [Recipe repo](https://github.com/zeropsio/recipes/tree/main/wasp-hello-world)
- [Reference demo app + zerops.yaml](https://github.com/zerops-recipe-apps/wasp-hello-world-app)
- [Zerops import.yaml reference](https://docs.zerops.io/references/import)
- [Zerops YAML preprocessor](https://docs.zerops.io/references/import-yaml/pre-processor)
- [Zerops zerops.yaml specification](https://docs.zerops.io/zerops-yaml/specification)
- [Wasp env vars](../../../deployment/env-vars.md)
- [Cloud provider deployment overview](../../../deployment/deployment-methods/cloud-providers.md)
