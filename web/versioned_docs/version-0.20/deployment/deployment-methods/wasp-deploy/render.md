---
title: Render
title-llm: Automated Deployment on Render with Wasp CLI
---

import { Required } from '@site/src/components/Tag';
import LaunchCommandEnvVars from './\_launch-command-env-vars.md'
import CiCdMention from './\_ci-cd-mention.md'

[Render](https://render.com/) is a unified cloud platform that makes it easy to build and run all your apps and websites with free TLS certificates, global CDN, private networks, and auto deploys from Git.

## Prerequisites

To deploy on Render using Wasp CLI:

1. Create a [Render](https://render.com/) account.

2. Ensure your Render account has access to create services and databases (free tier is sufficient to get started).

## Deploying

Using the Wasp CLI, you can easily deploy a new app on Render with a single command:

```shell
wasp deploy render launch my-wasp-app
```

<small>
  Please do not CTRL-C or exit your terminal while the commands are running.
</small>

Keep in mind that:

1. Your project name (for example `my-wasp-app`) must be unique across all your Render projects or deployment will fail.

2. If you are a member of multiple Render workspaces, the CLI will prompt you to select the workspace under which you want to deploy your app.

The project name is used as a base for your server and client service names on Render:

- `my-wasp-app-client`
- `my-wasp-app-server`

Render will create a PostgreSQL database service named `my-wasp-app-db`.

<LaunchCommandEnvVars />

If you have any additional environment variables that your app needs, read how to set them in the [API Reference](#render-environment-variables) section.

<CiCdMention />

## Using a Custom Domain For Your App {#custom-domain}

Setting up a custom domain is a three-step process:

1. Add your domain to the Render client service:

    - Go into the [Render dashboard](https://dashboard.render.com/).
    - Select your project (for example `my-wasp-app`).
    - Click on the client service (for example `my-wasp-app-client`).
    - Go to the **Settings** tab and scroll to **Custom Domain**.
    - Enter your domain name (for example `mycoolapp.com`).
    - Click **Add**.

2. Update the DNS records for your domain, adding a CNAME record at the domain or subdomain you want, pointing to the address you've been given in the previous step. _This step depends on your domain provider, consult their documentation in case of doubt._

3. To avoid [CORS](https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/CORS) errors, you need to set your new client URL as the `WASP_WEB_CLIENT_URL` environment variable (for example `https://mycoolapp.com`) for your **server service** in the Render dashboard.

    - Go into the [Render dashboard](https://dashboard.render.com/).
    - Select your project (for example `my-wasp-app`).
    - Click on the server service (for example `my-wasp-app-server`).
    - Go to the **Environment** tab.

    Update the `WASP_WEB_CLIENT_URL` variable with the new domain for your client.

That's it, your app should be available at `https://mycoolapp.com`!

## API Reference

### The `launch` command

`launch` is a convenience command that runs `setup` and `deploy` in sequence.

```shell
wasp deploy render launch <project-name>
```

It accepts the following arguments:

- `<project-name>` <Required />

  The name of your project.

Running `wasp deploy render launch` is the same as running the following commands:

```shell
wasp deploy render setup <project-name>
wasp deploy render deploy <project-name>
```

#### Explicitly providing the Render workspace

By default, Wasp CLI will prompt you to select a Render workspace for your project. If you want to skip the prompt and provide the workspace id or name directly, use the `--workspace` option:

```shell
wasp deploy render launch <project-name> --workspace <render-workspace-id-or-name>
```

#### Region Selection

By default, Render deploys services to the Oregon (US West) region. If you want to deploy to a different region, use the `--region` option:

```shell
wasp deploy render launch <project-name> --region frankfurt
```

Available regions:
- `oregon` (US West - default)
- `frankfurt` (EU Central)
- `singapore` (Asia Pacific)
- `ohio` (US East)
- `virginia` (US East)

#### Environment Variables {#render-launch-environment-variables}

##### Server

If you are deploying an app that requires any other environment variables (like social auth secrets), you can set them with the `--server-secret` option:

```
wasp deploy render launch my-wasp-app --server-secret GOOGLE_CLIENT_ID=<...> --server-secret GOOGLE_CLIENT_SECRET=<...>
```

##### Client

If you've added any [client-side environment variables](../../../project/env-vars.md#client-env-vars) to your app, pass them to the terminal session before running the `launch` command, for example:

```shell
REACT_APP_ANOTHER_VAR=somevalue wasp deploy render launch my-wasp-app
```

### The `deploy` command

The `deploy` command deploys your client and server apps on Render.

```shell
wasp deploy render deploy <project-name>
```

It accepts the following arguments:

- `<project-name>` <Required />

  The name of your project.

Run this command whenever you want to **update your deployed app** with the latest changes:

```shell
wasp deploy render deploy <project-name>
```

#### Other Available Options

- `--skip-client` - do not deploy the web client
- `--skip-server` - do not deploy the server

If you've added any [client-side environment variables](../../../project/env-vars.md#client-env-vars) to your app, pass them to the terminal session before running the `deploy` command, for example:

```shell
REACT_APP_ANOTHER_VAR=somevalue wasp deploy render deploy <project-name>
```

You must specify your client-side environment variables every time you redeploy with the above command [to ensure they are included in the build process](../../env-vars.md#client-env-vars).

### The `setup` command

The `setup` command creates your client, server, and database services on Render. It also configures environment variables. It does _not_ deploy the client or server services.

```shell
wasp deploy render setup <project-name>
```

It accepts the following arguments:

- `<project-name>`

  the name of your project.

The project name is used as a base for your server and client service names on Render:

- `<project-name>-client`
- `<project-name>-server`

Render also creates a PostgreSQL database service named `<project-name>-db`.

#### Explicitly providing the Render workspace

By default, Wasp CLI will prompt you to select in which Render workspace you want to create your project. If you want to skip the prompt and provide the workspace id or name directly, use the `--workspace` option:

```shell
wasp deploy render setup <project-name> --workspace <render-workspace-id-or-name>
```

:::caution Execute Only Once
You should only run `setup` once per app. Wasp CLI skips creating the services if they already exist.
:::

### Environment Variables {#render-environment-variables}

#### Server Secrets

If your app requires any other server-side environment variables (like social auth secrets), you can set them:

1. Initially in the `launch` or `setup` commands with the [`--server-secret` option](#render-launch-environment-variables)
2. After the app has already been deployed, go into the Render dashboard and set them in the **Environment** tab of your server service.

#### Client Environment Variables

If you've added any [client-side environment variables](../../../project/env-vars.md#client-env-vars) to your app, pass them to the terminal session before running a deployment command, for example:

```shell
REACT_APP_ANOTHER_VAR=somevalue wasp deploy render launch my-wasp-app
```

or

```shell
REACT_APP_ANOTHER_VAR=somevalue wasp deploy render deploy
```

Please note that you should do this for **every deployment**, not just the first time you set up the variables. One way to make sure you don't forget to add them is to create a `deploy` script in your `package.json` file:

```json title="package.json"
{
  "scripts": {
    "deploy": "REACT_APP_ANOTHER_VAR=somevalue wasp deploy render deploy"
  }
}
```

Then you can run `npm run deploy` to deploy your app.
