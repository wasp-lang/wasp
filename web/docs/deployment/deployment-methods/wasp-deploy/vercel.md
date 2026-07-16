---
title: Vercel
title-llm: Automated Deployment to Vercel with Wasp CLI
---

import { Required } from '@site/src/components/Tag';
import LaunchCommandEnvVars from './\_launch-command-env-vars.md'
import CiCdMention from './\_ci-cd-mention.md'

[Vercel](https://vercel.com/) is a cloud platform for building and deploying web apps, with zero-config support for a wide range of frameworks and managed integrations for databases and other services.

## Prerequisites

To deploy to Vercel using Wasp CLI:

1. Create a [Vercel](https://vercel.com/) account.

1. Install the [`vercel` CLI](https://vercel.com/docs/cli) on your machine and log in with `vercel login` (or pass an access token to each command with `--token`, see [Auth Options](#vercel-auth-options) below).

## Deploying

Using the Wasp CLI, you can easily deploy a new app to Vercel with a single command:

```shell
wasp deploy vercel launch my-wasp-app
```

<small>
  Please do not CTRL-C or exit your terminal while the commands are running.
</small>

Keep in mind that:

1. Your project name (for example `my-wasp-app`) must be unique across all your Vercel projects (within the team/scope you're deploying into) or deployment will fail.

1. If you are a member of multiple Vercel teams, pass the team you want to deploy into with the `--scope` option (see [Auth Options](#vercel-auth-options)).

The project name is used as a base for your server and client project names on Vercel:

- `my-wasp-app-server`
- `my-wasp-app-client`

The server runs as a standard Vercel Express app on [Fluid compute](https://vercel.com/docs/fluid-compute) - there's no Docker image or extra configuration to write, `launch`/`deploy` build and upload everything for you. The client is deployed as a static single-page app, with a rewrite rule so client-side routing keeps working on refresh/deep-links.

### Database: Neon by default {#database}

Unless you tell it otherwise, `wasp deploy vercel setup` (and therefore `launch`) provisions a [Neon](https://neon.tech/) Postgres database for your server app via the [Vercel Marketplace](https://vercel.com/marketplace) and wires up `DATABASE_URL` (pooled) and `DIRECT_URL` (direct, used for Prisma migrations) automatically.

The Neon integration has to be installed on your Vercel team once before the CLI can provision databases through it. If it isn't installed yet, the command will stop and tell you to either:

- run `vercel integration accept-terms neon` in an interactive terminal, or
- install it via the Vercel dashboard: your team -> **Storage** -> **Browse Marketplace** -> **Neon** -> **Install**.

Then re-run the command - it resumes automatically once the integration is installed.

#### Bringing your own database with `--database-url`

If you already have a Postgres database (Neon or otherwise) and don't want Wasp to provision one for you, pass `--database-url` to skip Neon provisioning entirely:

```shell
wasp deploy vercel launch my-wasp-app --database-url "postgresql://user:pass@host/db"
```

This sets `DATABASE_URL` directly on the server project. If your database needs a separate non-pooled connection string for migrations, pass it with `--direct-url`; otherwise the `--database-url` value is reused for `DIRECT_URL` as well:

```shell
wasp deploy vercel launch my-wasp-app \
  --database-url "postgresql://user:pass@host/db?pgbouncer=true" \
  --direct-url "postgresql://user:pass@host/db"
```

<LaunchCommandEnvVars />

If you have any additional environment variables that your app needs, read how to set them in the [API Reference](#vercel-launch-environment-variables) section.

<CiCdMention />

### Limitations: pg-boss jobs and WebSockets {#limitations}

Vercel's server runtime is serverless (Fluid compute scales your server to zero when it's idle), which doesn't fit two kinds of Wasp features well:

- **pg-boss jobs**: Wasp's [job](../../../advanced/jobs.md) executor (`pg-boss`) assumes a long-lived process to poll and run scheduled/recurring work. On a scale-to-zero platform, jobs can be delayed or missed entirely if there's no active invocation to run them.
- **WebSockets**: Wasp's [WebSocket](../../../advanced/web-sockets.md) support needs a persistent connection to a single server instance. [WebSockets on Vercel](https://vercel.com/docs/functions/functions-api-reference#websockets) are currently in beta: connections are pinned to one function instance (so they don't survive a scale-to-zero or a redeploy) and are bound by the function's `maxDuration`.

Because of this, `wasp deploy vercel setup`, `deploy`, and `launch` all run a best-effort **preflight check** before doing anything else: they scan your `main.wasp`/`main.wasp.ts` (and its local imports) for job and WebSocket usage and print a `WARN:` message to the terminal if it finds any. This is a **warn-not-block** check - it never fails the command or changes its exit code, it's purely informational.

If you see one of these warnings:

- For jobs, consider replacing `pg-boss` with [Vercel Cron](https://vercel.com/docs/cron-jobs) to trigger the same work on a schedule instead of relying on a long-lived worker.
- For WebSockets, either accept the beta limitations above, or deploy the server on a provider with a long-lived process (for example [Railway](./railway.md) or [Fly.io](./fly.md)) if you need production-grade WebSocket support today.

## API Reference

### The `launch` command

`launch` is a convenience command that runs `setup` and `deploy` in sequence.

```shell
wasp deploy vercel launch <project-name>
```

It accepts the following arguments:

- `<project-name>` <Required />

  The name of your project.

Running `wasp deploy vercel launch` is the same as running the following commands:

```shell
wasp deploy vercel setup <project-name>
wasp deploy vercel deploy <project-name>
```

It accepts the same options as `setup` (see below), since `launch` forwards them straight through.

#### Environment Variables {#vercel-launch-environment-variables}

##### Server

If you are deploying an app that requires any other environment variables (like social auth secrets), you can set them with the `--server-secret` option:

```shell
wasp deploy vercel launch my-wasp-app --server-secret GOOGLE_CLIENT_ID=<...> --server-secret GOOGLE_CLIENT_SECRET=<...>
```

##### Client

If you've added any [client-side environment variables](../../../project/env-vars.md#client-env-vars) to your app, pass them to the terminal session before running the `launch` command, for example:

```shell
REACT_APP_ANOTHER_VAR=somevalue wasp deploy vercel launch my-wasp-app
```

### The `setup` command

The `setup` command creates your client and server projects on Vercel, provisions the database, and configures environment variables. It does _not_ deploy the client or server apps.

```shell
wasp deploy vercel setup <project-name>
```

It accepts the following arguments:

- `<project-name>` <Required />

  The name of your project.

The project name is used as a base for your server and client project names on Vercel:

- `<project-name>-server`
- `<project-name>-client`

By default `setup` provisions a Neon database via the Vercel Marketplace - see [Database: Neon by default](#database) above for the `--db`/`--database-url`/`--direct-url` options.

#### Other options

- `--server-secret <NAME=VALUE>` - additional server-side environment variable to set (repeatable). See [Environment Variables](#vercel-launch-environment-variables).
- `--db <db>` - managed database to provision via the Vercel Marketplace. Only `neon` is supported (this is also the default), see [Database: Neon by default](#database).
- `--database-url <url>` - skip database provisioning and use this connection string as `DATABASE_URL`, see [Bringing your own database](#database).
- `--direct-url <url>` - non-pooled connection string for `DIRECT_URL`, only used together with `--database-url`. Defaults to the `--database-url` value if omitted.

See also [Auth Options](#vercel-auth-options) below.

:::caution Execute Only Once
You should only run `setup` once per app. Wasp CLI skips creating the projects/database if they already exist.
:::

### The `deploy` command

The `deploy` command builds your Wasp app and deploys the client and server to Vercel.

```shell
wasp deploy vercel deploy <project-name>
```

It accepts the following arguments:

- `<project-name>` <Required />

  The name of your project.

Run this command whenever you want to **update your deployed app** with the latest changes:

```shell
wasp deploy vercel deploy <project-name>
```

If you run `deploy` before `setup` has created the projects, it fails and tells you to run `setup` (or `launch`) first.

If you've added any [client-side environment variables](../../../project/env-vars.md#client-env-vars) to your app, pass them to the terminal session before running the `deploy` command, for example:

```shell
REACT_APP_ANOTHER_VAR=somevalue wasp deploy vercel deploy my-wasp-app
```

You must specify your client-side environment variables every time you redeploy with the above command [to ensure they are included in the build process](../../env-vars.md#client-env-vars).

See also [Auth Options](#vercel-auth-options) below.

### Auth Options {#vercel-auth-options}

All three commands (`setup`, `deploy`, `launch`) accept the same authentication options:

- `--token <token>` - a Vercel access token. Defaults to your logged-in `vercel` CLI session (`vercel login`) if omitted. Useful for CI/CD, see the [CI/CD Deployment](./ci-cd.md) page.
- `--scope <scope>` - the Vercel team (slug or ID) to operate in, if your account belongs to more than one team.

```shell
wasp deploy vercel launch my-wasp-app --token <vercel-token> --scope my-team
```

### Environment Variables {#vercel-environment-variables}

#### Server Secrets

If your app requires any other server-side environment variables (like social auth secrets), you can set them:

1. Initially in the `launch` or `setup` commands with the [`--server-secret` option](#vercel-launch-environment-variables)
2. After the app has already been deployed, go into the Vercel dashboard and set them under your server project's **Settings** -> **Environment Variables** tab.

#### Client Environment Variables

If you've added any [client-side environment variables](../../../project/env-vars.md#client-env-vars) to your app, pass them to the terminal session before running a deployment command, for example:

```shell
REACT_APP_ANOTHER_VAR=somevalue wasp deploy vercel launch my-wasp-app
```

or

```shell
REACT_APP_ANOTHER_VAR=somevalue wasp deploy vercel deploy my-wasp-app
```

Please note that you should do this for **every deployment**, not just the first time you set up the variables. One way to make sure you don't forget to add them is to create a `deploy` script in your `package.json` file:

```json title="package.json"
{
  "scripts": {
    "deploy": "REACT_APP_ANOTHER_VAR=somevalue wasp deploy vercel deploy my-wasp-app"
  }
}
```

Then you can run `npm run deploy` to deploy your app.
