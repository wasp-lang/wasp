---
comments: true
---

import LastCheckedWithVersionsNotice from "@site/src/components/LastCheckedWithVersionsNotice";
import AddExternalAuthEnvVarsReminder from './_addExternalAuthEnvVarsReminder.md'
import { Server, Client, Database } from '../DeploymentTag'

# SnapDeploy

<LastCheckedWithVersionsNotice versions={{ Wasp: "0.25", SnapDeploy: new Date("2026-10-21") }} />

## Deploy Wasp on SnapDeploy <Server /> <Client /> <Database />

This guide shows you how to deploy the server and the client as two containers from one Git repository, with a managed PostgreSQL database next to them, on [SnapDeploy](https://snapdeploy.dev).

Like Render, SnapDeploy builds from source on its own servers: a Dockerfile in your repository installs the Wasp CLI and runs `wasp build`, so you don't commit `.wasp/out` and every push to the branch redeploys. Unlike Render, both containers are plain Docker images, so what you see building locally is what runs.

### Prerequisites

To get started, follow these steps:

1. Create a [SnapDeploy](https://snapdeploy.dev/register) account. The free tier is enough to try the deployment; for a real app, run the server on [Always-On](https://snapdeploy.dev/pricing) ($12/month) and use the managed PostgreSQL add-on (from $29/month).
1. Push your Wasp project to a GitHub repository and connect GitHub to SnapDeploy (Settings → GitHub).
1. [Switch the database to PostgreSQL](../../../features/data/databases.md#migrating-from-sqlite-to-postgresql), generate your initial migrations locally with `wasp db migrate-dev` and commit the `migrations/` directory. The server applies them on every start.

### Add the Dockerfiles

Create these three files in the root of your repository.

The server image mirrors the Dockerfile Wasp generates in `.wasp/out`: a build stage that installs the Wasp CLI and builds the app, and a runtime stage. Two stages in total, which is what SnapDeploy's free Small container allows. It uses Debian-based images because the Wasp CLI ships glibc binaries.

```dockerfile title="Dockerfile"
ARG WASP_VERSION=0.25.0
ARG NODE_IMAGE=node:24-bookworm-slim

FROM ${NODE_IMAGE} AS builder
ARG WASP_VERSION
RUN apt-get update \
 && apt-get install -y --no-install-recommends openssl ca-certificates python3 build-essential \
 && rm -rf /var/lib/apt/lists/* \
 && npm install -g @wasp.sh/wasp-cli@${WASP_VERSION}
WORKDIR /app
COPY . .
RUN wasp install && wasp build
RUN cd .wasp/out/server \
 && npm install \
 && npx prisma generate --schema=../db/schema.prisma \
 && npm run bundle \
 && mkdir -p node_modules

FROM ${NODE_IMAGE} AS runtime
RUN apt-get update \
 && apt-get install -y --no-install-recommends openssl ca-certificates \
 && rm -rf /var/lib/apt/lists/*
ENV NODE_ENV=production
WORKDIR /app
COPY --from=builder /app/node_modules ./node_modules
COPY --from=builder /app/.wasp/out/sdk ./.wasp/out/sdk
COPY --from=builder /app/.wasp/out/server/node_modules ./.wasp/out/server/node_modules
COPY --from=builder /app/.wasp/out/server/bundle ./.wasp/out/server/bundle
COPY --from=builder /app/.wasp/out/server/package*.json ./.wasp/out/server/
COPY --from=builder /app/.wasp/out/db ./.wasp/out/db
WORKDIR /app/.wasp/out/server
EXPOSE 3001
ENTRYPOINT ["npm", "run", "start-production"]
```

The client image builds the web app with Vite and serves it with nginx, with the single-page-app fallback that client-side routes need:

```dockerfile title="client.Dockerfile"
ARG WASP_VERSION=0.25.0
ARG NODE_IMAGE=node:24-bookworm-slim

FROM ${NODE_IMAGE} AS builder
ARG WASP_VERSION
ARG REACT_APP_API_URL
RUN apt-get update \
 && apt-get install -y --no-install-recommends openssl ca-certificates python3 build-essential \
 && rm -rf /var/lib/apt/lists/* \
 && npm install -g @wasp.sh/wasp-cli@${WASP_VERSION}
WORKDIR /app
COPY . .
RUN wasp install && wasp build
RUN test -n "$REACT_APP_API_URL" || (echo "REACT_APP_API_URL is not set" && exit 1)
RUN REACT_APP_API_URL=${REACT_APP_API_URL} npx vite build

FROM nginx:1.27-alpine
COPY nginx.conf /etc/nginx/conf.d/default.conf
RUN rm -rf /usr/share/nginx/html/*
COPY --from=builder /app/.wasp/out/web-app/build /usr/share/nginx/html
EXPOSE 80
```

Wasp's client build emits `200.html` as the single-page shell, plus an `index.html` per route only if you use the `prerender` option. nginx prefers a prerendered `index.html` where one exists and falls back to `200.html` for everything else. `COPY` merges into the image's html directory, so the stock welcome page is removed first:

```nginx title="nginx.conf"
server {
    listen       80;
    server_name  _;
    root         /usr/share/nginx/html;
    index        index.html 200.html;
    location /assets/ {
        add_header Cache-Control "public, max-age=31536000, immutable";
        try_files $uri =404;
    }
    location / {
        try_files $uri $uri/index.html /200.html;
    }
}
```

Add a `.dockerignore` with `node_modules`, `.wasp`, `.git` and `.env.*` so the build context is the source only. Then commit and push:

```bash
git add Dockerfile client.Dockerfile nginx.conf .dockerignore
git commit -m "Add SnapDeploy Dockerfiles"
git push origin main
```

### Create the Database

In the SnapDeploy dashboard go to **Add-ons → Add Database → PostgreSQL** and pick a plan. Mini ($29/month) is enough for most apps: a managed PostgreSQL with daily backups, next to your containers. Note the connection string on the add-on's credentials page, or link the add-on to the server container in the next step and `DATABASE_URL` is injected for you.

### Deploy the Server

1. **Deploy Your Application → GitHub**, pick the repository and branch. The `Dockerfile` is detected and port `3001` is read from its `EXPOSE` line; SnapDeploy injects `PORT=3001`, which the Wasp server reads.
1. Pick a container name (it becomes the URL `https://<server-name>.containers.snapdeploy.app`).
1. Set the environment variables before the first build:

| Variable | Value |
|---|---|
| `DATABASE_URL` | injected by the linked PostgreSQL add-on, or the connection string from its credentials page |
| `JWT_SECRET` | a random string of at least 32 characters |
| `WASP_SERVER_URL` | `https://<server-name>.containers.snapdeploy.app` |
| `WASP_WEB_CLIENT_URL` | the client URL from the next section; any placeholder for the first build |

<AddExternalAuthEnvVarsReminder />

1. Click **Deploy**. The build installs the Wasp CLI, compiles the app and bundles the server, which takes a few minutes the first time. When the container starts it runs `prisma migrate deploy` and answers at its URL.

### Deploy the Client

SnapDeploy links one container per repository branch, so the client deploys from a second branch of the same repository. Create it once and keep it equal to `main`:

```bash
git push origin main:client
```

1. **Deploy Your Application → GitHub**, the same repository, branch `client`, then open **Advanced Options** and set **Dockerfile Path** to `client.Dockerfile`.
1. Set the environment variable before the first build:

| Variable | Value |
|---|---|
| `REACT_APP_API_URL` | `https://<server-name>.containers.snapdeploy.app` |

:::caution
`REACT_APP_API_URL` must be set **before** the client build runs. SnapDeploy passes `REACT_APP_*` variables into the Docker build as build arguments and Vite embeds the value into the compiled JavaScript. If you change it later, redeploy the client; a restart is not enough.
:::

1. Click **Deploy**. The client is served by nginx at `https://<client-name>.containers.snapdeploy.app`.

### Connect the Two

Go back to the server container, set `WASP_WEB_CLIENT_URL` to the client's URL and redeploy. Open the client URL and sign up.

### Redeploying After Changes

A push to `main` rebuilds the server; `git push origin main:client` rebuilds the client. If you add database model changes, run `wasp db migrate-dev` locally first and commit the generated migration files with your code; the server applies them on its next start.

:::note Free tier
Free containers sleep after 15 minutes without traffic. A browser visit wakes the client, but the client's API calls do not wake a sleeping server (the wake page only runs in a browser), so for anything beyond a demo run the server on SnapDeploy's [Always-On](https://snapdeploy.dev/pricing) plan ($12/month per container: no sleep, and a custom domain if you want one) and leave the client on the free tier. A worked example with these exact files is at [github.com/AAR-Labs/wasp-on-snapdeploy](https://github.com/AAR-Labs/wasp-on-snapdeploy).
:::
