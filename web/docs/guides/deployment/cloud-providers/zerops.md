---
comments: true
---

import LastCheckedWithVersionsNotice from "@site/src/components/LastCheckedWithVersionsNotice";
import AddExternalAuthEnvVarsReminder from './_addExternalAuthEnvVarsReminder.md'
import { SecretGeneratorBlock } from '../../../project/SecretGeneratorBlock'
import { Server, Client, Database } from '../DeploymentTag'

# Zerops

<LastCheckedWithVersionsNotice versions={{ Wasp: "0.25", Zerops: new Date("2026-08-24") }} />

## Deploy Wasp on Zerops <Server /> <Client /> <Database />

This guide shows you how to deploy the server, the client, and provision a database on [Zerops](https://zerops.io/).

Like Render, Zerops builds your Wasp app from source on its servers, so you don't need to run `wasp build` locally before deploying. You'll define the project topology in an `import.yaml` and the build/deploy pipeline in a `zerops.yaml`.

The fastest path is the official [Wasp Hello World recipe](https://app.zerops.io/recipes/wasp-hello-world), which imports those files for you. To deploy your own app, copy the same config into your repository — both are shown below.

The live recipe files live in the [recipe repo](https://github.com/zeropsio/recipes/tree/main/wasp-hello-world) (`import.yaml` per environment) and the [demo app](https://github.com/zerops-recipe-apps/wasp-hello-world-app) (`zerops.yaml`).

### Prerequisites

To get started, follow these steps:

1. Create a [Zerops](https://zerops.io/) account.
1. If you are deploying **your own** Wasp app, push it to a Git repository and generate the initial database migrations locally by running `wasp db migrate-dev`. Commit the `migrations/` directory — Zerops needs those files in the repo to set up your database.

### Deploy with the official recipe

1. Open the [Wasp Hello World recipe](https://app.zerops.io/recipes/wasp-hello-world) and pick an environment (see [Recipe environments](#recipe-environments)).
1. Click **Deploy** — Zerops imports the project and starts building `db`, then `api`, then the client (`app`, or `appstage` on AI Agent / Remote).
1. When builds finish, open the deployed client URL. On the hello-world demo, log in with **demo** / **demo-zerops1** (seeded on first boot).

### Recipe environments

Each environment is a separate Zerops project. You can deploy more than one (for example **Stage** for QA and **Small Production** for live traffic).

| Environment | Best for | What you get |
|-------------|----------|--------------|
| [**AI Agent**](https://app.zerops.io/recipes/wasp-hello-world?environment=ai-agent) | Coding agents | SSH into `appdev` / `apidev` and run `wasp start`, plus staged client/API URLs |
| [**Remote (CDE)**](https://app.zerops.io/recipes/wasp-hello-world?environment=remote-cde) | Cloud dev environment | Same layout as AI Agent, tuned for a human developer over SSH |
| [**Local**](https://app.zerops.io/recipes/wasp-hello-world?environment=local) | Laptop + cloud DB | Production client/API on Zerops; you run `wasp start` locally over [zCLI VPN](#local-development) |
| [**Stage**](https://app.zerops.io/recipes/wasp-hello-world?environment=stage) | Pre-production / QA | One client + API + DB using the production build pipeline |
| [**Small Production**](https://app.zerops.io/recipes/wasp-hello-world?environment=small-production) | Small live apps | Same pipeline, two containers each for zero-downtime deploys |
| [**Highly-available Production**](https://app.zerops.io/recipes/wasp-hello-world?environment=highly-available-production) | Production with HA | Scaled client/API plus HA PostgreSQL |

### Architecture

Production-style environments (**Stage**, **Small Production**, **HA**, **Local**) map Wasp's split to three Zerops services:

| Wasp piece | Zerops service | Role |
|------------|----------------|------|
| React SPA | `app` — `static` | Built client served by Nginx over HTTPS |
| Node.js API + Prisma | `api` — `nodejs@24` | Server, migrations, auth |
| PostgreSQL | `db` — `postgresql:single@16` | Managed database |

AI Agent and Remote add **`appdev`** and **`apidev`** containers that run the `dev` setup from `zerops.yaml` (full source, SSH, `wasp start`). Their preview client is `appstage`, not `app`.

### Create the zerops.yaml

Create a `zerops.yaml` in the root of your repository. Stage and Small Production both use these two setups — only the `import.yaml` scaling differs.

```yaml title="zerops.yaml"
# yaml-language-server: $schema=https://api.app-prg1.zerops.io/api/rest/public/settings/zerops-yaml-json-schema.json

zerops:
  # React client — Wasp + Vite build, then Nginx serves the static files
  - setup: prod-client
    build:
      base: nodejs@24
      buildCommands:
        - npm install -g @wasp.sh/wasp-cli@0.25
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
        - npm install -g @wasp.sh/wasp-cli@0.25
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

The official [demo `zerops.yaml`](https://github.com/zerops-recipe-apps/wasp-hello-world-app/blob/main/zerops.yaml) follows this same pipeline. It uses `npx wasp` from the demo app's `@wasp.sh/wasp-cli` dependency, helper scripts to assemble the deploy tree, and a second `zsc execOnce` that seeds the **demo** / **demo-zerops1** user on first boot. You don't need that seed for your own app.

#### What the pipeline does

1. **Install Wasp and compile from source** — `wasp install && wasp build` (same idea as [Render](./render.md)).
1. **Client (`prod-client`)** — `npx vite build` with `REACT_APP_API_URL` set, then the static files are deployed to an Nginx (`static`) service.
1. **API (`prod-api`)** — `npm install` in `.wasp/out/server`, `prisma generate`, `npm run bundle`, then the Node server starts on port **3001**.
1. **First boot of a new API version** — `zsc execOnce` runs `prisma migrate deploy` once per new app version. The recipe also seeds a demo user the same way.

### Create the import.yaml

`import.yaml` is the project blueprint: which services exist, how they scale, and the **value store** (generic `APP_URL` / `API_URL` / `APP_SECRET`). `zerops.yaml` maps those into Wasp's env var names. Do not put `envVariables` on service blocks in `import.yaml`.

This is the **Small Production** topology (two containers each). Stage is the same services with one container each.

```yaml title="import.yaml"
#yamlPreprocessor=on
# yaml-language-server: $schema=https://api.app-prg1.zerops.io/api/rest/public/settings/import-project-yaml-json-schema.json

project:
  name: my-wasp-app-small-prod
  envVariables:
    APP_URL: https://app-${zeropsSubdomainHost}.prg1.zerops.app
    API_URL: https://api-${zeropsSubdomainHost}-3001.prg1.zerops.app
  envSecrets:
    # Generated once at import — equivalent to Render's generateValue: true
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
|---|---|---|
| `<your-org>/<your-wasp-app>` | Your GitHub repository | `my-org/my-wasp-app` |
| `project.name` | A unique name for this Zerops project | `my-wasp-app-small-prod` |

The `zeropsSubdomainHost` placeholder is filled in by Zerops at import time so the client and API get matching HTTPS subdomains. `zeropsSetup` on each service must match a `setup:` name in `zerops.yaml`.

The recipe's [Small Production `import.yaml`](https://github.com/zeropsio/recipes/blob/main/wasp-hello-world/4%20%E2%80%94%20Small%20Production/import.yaml) is this same file, pointed at the demo app repo.

### Environment variables

Zerops has two layers. The project **value store** is set once (recipe import or your `import.yaml`) and is editable in the dashboard. `zerops.yaml` maps those values into the names Wasp expects.

#### Project value store (`import.yaml`)

| Variable | Purpose |
|---|---|
| `APP_URL` | Public client URL (`https://app-…zerops.app`) |
| `API_URL` | Public API URL (`https://api-…-3001.zerops.app`) |
| `APP_SECRET` | Random secret generated at import with the YAML preprocessor. Requires `#yamlPreprocessor=on` as the first line of `import.yaml`. |

#### Mapped into the client build (`prod-client`)

| Variable | Value | When |
|---|---|---|
| `REACT_APP_API_URL` | `${API_URL}` | **Build time** — Vite embeds it into the compiled JavaScript |

:::caution
`REACT_APP_API_URL` must be set **before** the client build runs. If it's missing, all API calls from the client will fail.
:::

#### Mapped into the API runtime (`prod-api`)

| Variable | Value | Notes |
|---|---|---|
| `DATABASE_URL` | `postgresql://${db_user}:${db_password}@${db_hostname}:${db_port}/${db_dbName}` | Zerops injects the `db` service credentials automatically |
| `WASP_SERVER_URL` | `${API_URL}` | Public API URL, including `https://` |
| `WASP_WEB_CLIENT_URL` | `${APP_URL}` | Public client URL, including `https://` |
| `JWT_SECRET` | `${APP_SECRET}` | At least 32 characters. Comes from the project value store — do not hardcode it |
| `PORT` | `3001` | Wasp server port (must match `API_URL` and the readiness check) |

<AddExternalAuthEnvVarsReminder />

Because `zerops.yaml` maps `JWT_SECRET` to `${APP_SECRET}`, you must supply `APP_SECRET` in the project value store. Setting `JWT_SECRET` in the dashboard will not work — runtime `envVariables` in `zerops.yaml` take precedence over dashboard secrets.

Generate `APP_SECRET` at import with the YAML preprocessor (as shown in `import.yaml` above), or add it manually in the dashboard (**Project → Environment variables → Secrets**) as a random string at least 32 characters long:<br /><SecretGeneratorBlock />

The hello-world recipe demo hardcodes `JWT_SECRET` in its `zerops.yaml` so the sample login works out of the box. If you reuse a recipe project for your own app, add `APP_SECRET` in the dashboard before pointing `buildFromGit` at your repository.

Do not self-reference Wasp keys in `zerops.yaml` (for example `${WASP_SERVER_URL}`). Those names are created by the mapping; only the value-store keys (`APP_URL`, `API_URL`, `APP_SECRET`) and Zerops service keys (`${db_password}`, …) resolve.

### Deploy your own app

1. Commit `zerops.yaml` (and, if you import from the dashboard, keep `import.yaml` handy).
1. In the Zerops dashboard, create a project and **Import** the `import.yaml`, or run `zcli project project-import`.
1. Point `buildFromGit` at your repository (or connect the GitHub / GitLab integration so later pushes rebuild automatically).
1. Wait for `db`, then `api`, then `app` to finish building.

If you started from the recipe, you can keep that project and switch `buildFromGit` (or the Git connection) to your own repo — you don't have to re-import. Before doing so, add `APP_SECRET` to the project value store (see [Environment variables](#environment-variables)) — the recipe's `import.yaml` does not include it, but your `zerops.yaml` needs it for `JWT_SECRET`.

### Verify your deployment

- **Client:** open the `app` URL (or `appstage` on AI Agent / Remote) — the SPA loads.
- **API:** `GET /auth/me` on port 3001 responds (this is the readiness check).
- **Auth:** if login fails, check that `WASP_WEB_CLIENT_URL` and `WASP_SERVER_URL` match the public client and API URLs in the project value store.

### Redeploying After Changes

When the `app` and `api` services are connected to your Git repository, push to the linked branch:

```bash
git push origin main
```

Zerops starts a new build of each connected service. You can also trigger a rebuild from the dashboard or with `zcli service trigger-build`.

If you changed the Prisma schema, run `wasp db migrate-dev` locally first and commit the generated files in `migrations/` along with your code. On the next API deploy, `zsc execOnce` runs `prisma migrate deploy` once for that new app version before the server starts.

Changing `APP_URL` or `API_URL` in the project value store requires a **rebuild** of the client (so Vite can embed the new `REACT_APP_API_URL`) and a **restart** of the API (so `WASP_SERVER_URL` / `WASP_WEB_CLIENT_URL` update).

### Local development

The [**Local**](https://app.zerops.io/recipes/wasp-hello-world?environment=local) environment deploys client, API, and PostgreSQL on Zerops while you run `wasp start` on your machine.

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

- [Wasp Hello World recipe](https://app.zerops.io/recipes/wasp-hello-world) — deploy with one click
- [Recipe repo](https://github.com/zeropsio/recipes/tree/main/wasp-hello-world) — `import.yaml` per environment
- [Reference demo app + zerops.yaml](https://github.com/zerops-recipe-apps/wasp-hello-world-app)
- [Zerops import.yaml reference](https://docs.zerops.io/references/import)
- [Zerops YAML preprocessor](https://docs.zerops.io/references/import-yaml/pre-processor) — `<@generateRandomString>` for secrets
- [Zerops zerops.yaml specification](https://docs.zerops.io/zerops-yaml/specification)
- [Wasp env vars](../../../deployment/env-vars.md)
- [Cloud provider deployment overview](../../../deployment/deployment-methods/cloud-providers.md)
