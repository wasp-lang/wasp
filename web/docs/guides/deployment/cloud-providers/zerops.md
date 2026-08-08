---
comments: true
---

import LastCheckedWithVersionsNotice from "@site/src/components/LastCheckedWithVersionsNotice";
import { Server, Client, Database } from '../DeploymentTag'

# Zerops

<LastCheckedWithVersionsNotice versions={{ Wasp: "0.25", Zerops: new Date("2026-08-01") }} />

## Deploy Wasp on Zerops <Server /> <Client /> <Database />

Deploy a full-stack Wasp app on [Zerops](https://zerops.io/) with one click. The official [Wasp Hello World recipe](https://app.zerops.io/recipes/wasp-hello-world) creates everything for you — PostgreSQL database, Node.js API, static React client, HTTPS subdomains, build pipelines, and all required environment variables.

### Deploy with one click

1. Open the [Wasp Hello World recipe](https://app.zerops.io/recipes/wasp-hello-world) and pick an environment (see below).
1. Click **Deploy** — Zerops imports the project and starts building `db`, then `api`, then `app`.
1. Open the `app` URL when builds finish. Demo login: **demo** / **demo-zerops1** (seeded on first deploy).

### Recipe environments

The recipe covers the full development lifecycle — from AI-assisted coding to production. Each environment is a ready-made Zerops project with its own `import.yaml`, linked to the [demo app repo](https://github.com/zerops-recipe-apps/wasp-hello-world-app) and its `zerops.yaml` build pipelines.

| Environment | Best for | What you get |
|-------------|----------|--------------|
| [**AI Agent**](https://app.zerops.io/recipes/wasp-hello-world?environment=ai-agent) | Coding agents (Claude Code, Cursor, opencode, etc.) | Deploy and start developing immediately — SSH into dev containers, run `wasp start`, with PostgreSQL already running. Staged client and API URLs for preview when you're ready. |
| [**Remote (CDE)**](https://app.zerops.io/recipes/wasp-hello-world?environment=remote-cde) | Remote development (cloud dev environment) | Same dev + stage layout as AI Agent, tuned for a human developer coding over SSH or a mounted remote filesystem — `wasp start` on dev containers, production-like stage URLs for testing. |
| [**Local**](https://app.zerops.io/recipes/wasp-hello-world?environment=local) | Local `wasp start` with a cloud database | Deployed client, API, and PostgreSQL on Zerops. Run `wasp start` on your machine and connect to the cloud DB via [zCLI VPN](#local-development). |
| [**Stage**](https://app.zerops.io/recipes/wasp-hello-world?environment=stage) | Pre-production testing | Single client + API + DB using production build pipelines on minimal resources — one container each, good for demos and QA before going live. |
| [**Small Production**](https://app.zerops.io/recipes/wasp-hello-world?environment=small-production) | Small live apps | Production client and API with two containers each for zero-downtime deploys, plus a managed PostgreSQL database. |
| [**Highly-available Production**](https://app.zerops.io/recipes/wasp-hello-world?environment=highly-available-production) | Production with HA database | Scaled client/API containers (2–6 API replicas) and HA PostgreSQL — for apps that need redundancy and autoscaling headroom. |

Pick the environment that matches where you are in the lifecycle. You can deploy multiple environments as separate Zerops projects (e.g. **Stage** for QA and **Small Production** for live traffic).

### Architecture

Every production-style environment (`Stage`, `Small Production`, `HA`) maps Wasp's split to Zerops services:

| Wasp piece | Zerops service | Role |
|------------|----------------|------|
| React SPA | `app` — static | Built client served over HTTPS |
| Node.js API + Prisma | `api` — `nodejs@24` | Server, migrations, auth |
| PostgreSQL | `db` — `postgresql@16` | Managed database |

Dev environments (`AI Agent`, `Remote`) add **`appdev`** and **`apidev`** containers running the `dev` setup from `zerops.yaml` — full source code with SSH access for `wasp start` and live iteration.

### Verify your deployment

- **Client:** open the `app` (or `appstage`) URL — the SPA loads.
- **API:** `GET /auth/me` on port 3001 responds (used as the readiness check).
- **Auth:** log in with **demo** / **demo-zerops1**. If login fails, check that `WASP_WEB_CLIENT_URL` and `WASP_SERVER_URL` are set correctly in the project (the recipe handles this automatically).

### Local development

The [**Local**](https://app.zerops.io/recipes/wasp-hello-world?environment=local) environment deploys client, API, and PostgreSQL on Zerops while you run `wasp start` on your machine:

```bash
zcli vpn up
wasp start   # DATABASE_URL points at the Zerops db via VPN
```

Install [zCLI](https://docs.zerops.io/references/cli) if you haven't already.

### Resources

- [Wasp Hello World recipe](https://app.zerops.io/recipes/wasp-hello-world) — deploy with one click
- [Recipe repo](https://github.com/zeropsio/recipes/tree/main/wasp-hello-world) — `import.yaml` per environment
- [Reference demo app + zerops.yaml](https://github.com/zerops-recipe-apps/wasp-hello-world-app)
- [Zerops import.yaml reference](https://docs.zerops.io/references/import)
- [Zerops zerops.yaml specification](https://docs.zerops.io/zerops-yaml/specification)
- [Wasp env vars](../../../deployment/env-vars.md)
- [Cloud provider deployment overview](../../../deployment/deployment-methods/cloud-providers.md)
