---
title: Railway
---

import { Required } from '@site/src/components/Tag';
import LaunchCommandEnvVars from './\_launch-command-env-vars.md'
import CustomPostgresOption from './\_railway-custom-postgres-option.md'
import CiCdMention from './\_ci-cd-mention.md'
import CustomServerUrlOption from './\_custom-server-url-option.md'

[Railway](https://railway.com/?utm_medium=integration&utm_source=docs&utm_campaign=wasp) is a cloud development platform that streamlines building and deploying applications with built-in support for databases and services. It offers an intuitive interface and automates infrastructure.

## Prerequisites

To deploy to Railway using Wasp CLI:

1. Create a [Railway](https://railway.com/?utm_medium=integration&utm_source=docs&utm_campaign=wasp) account,

1. Install the [`railway` CLI](https://docs.railway.com/guides/cli?utm_medium=integration&utm_source=docs&utm_campaign=wasp#installing-the-cli) on your machine.

## Deploying

Using the Wasp CLI, you can easily deploy a new app to Railway with a single command:

```shell
wasp deploy railway launch my-wasp-app
```

<small>
  Please do not CTRL-C or exit your terminal while the commands are running.
</small>

Keep in mind that:

1. Your project name (for example `my-wasp-app`) must be unique across all your Railway projects or deployment will fail (this is a current limitation of the Wasp CLI and Railway integration [#2926](https://github.com/wasp-lang/wasp/issues/2926)).

1. If you are a member of multiple Railway organizations, the CLI will prompt you to select the organization under which you want to deploy your app.

<Tabs groupId="deployment-mode">
<TabItem value="single" label="Single deployment">

The project name is used as a base for your server service name on Railway:

- `my-wasp-app-server`, which runs the server and serves the client.

The database service is always named `Postgres`, regardless of the project name.

:::note Upgrading a project deployed before 0.26
Earlier Wasp versions also created a `my-wasp-app-client` service. Wasp no longer deploys it, but it does not delete it either. After your first deploy with the new version, set `WASP_WEB_CLIENT_URL` on the server service to the server's own URL, re-register your OAuth redirect URIs on the server URL, and delete the client service in the Railway dashboard. See the [migration guide](../../../migration-guide.md).
:::

</TabItem>
<TabItem value="split" label="Split deployment">

The project name is used as a base for your server and client service names on Railway:

- `my-wasp-app-client`, where your app is available
- `my-wasp-app-server`

The database service is always named `Postgres`, regardless of the project name.

</TabItem>
</Tabs>

<LaunchCommandEnvVars />

If you have any additional environment variables that your app needs, read how to set them in the [API Reference](#railway-environment-variables) section.

<CiCdMention />

## Using a Custom Domain For Your App {#custom-domain}

Setting up a custom domain is a three-step process:

1. Add your domain to the Railway service your users visit: the server service in the default single deployment mode, the client service in [split mode](../../intro.md#deployment-modes).

    - Go into the [Railway dashboard](https://railway.com/dashboard?utm_medium=integration&utm_source=docs&utm_campaign=wasp).
    - Select your project (for example `my-wasp-app`).
    - Click on that service (for example `my-wasp-app-server`).
    - Go to the **Settings** tab and click **Custom Domain**.
    - Enter your domain name (for example `mycoolapp.com`) and port `8080`.
    - Click **Add Domain**.

2. Update the DNS records for your domain, adding a CNAME record at the domain or subdomain you want, pointing to the address you've been given in the previous step. _This step depends on your domain provider, consult their documentation in case of doubt._

3. Tell the server about your new domain through the environment variables of your **server service** in the Railway dashboard.

    - Go into the [Railway dashboard](https://railway.com/dashboard?utm_medium=integration&utm_source=docs&utm_campaign=wasp).
    - Select your project (for example `my-wasp-app`).
    - Click on the server service (for example `my-wasp-app-server`).
    - Go to the **Variables** tab.

<Tabs groupId="deployment-mode">
<TabItem value="single" label="Single deployment">

Set both `WASP_SERVER_URL` and `WASP_WEB_CLIENT_URL` to the new domain (for example `https://mycoolapp.com`). Wasp uses them to build OAuth redirect URIs and email links, so if you use OAuth, update the redirect URI with your provider to `https://mycoolapp.com/auth/<provider>/callback`.

</TabItem>
<TabItem value="split" label="Split deployment">

Set `WASP_WEB_CLIENT_URL` to the new client domain (for example `https://mycoolapp.com`), so that [CORS](https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/CORS) allows it. The server keeps its own URL, so the OAuth redirect URI doesn't change.

</TabItem>
</Tabs>

That's it, your app should be available at `https://mycoolapp.com`!

## API Reference

### The `launch` command

`launch` is a convenience command that runs `setup` and `deploy` in sequence.

```shell
wasp deploy railway launch <project-name>
```

It accepts the following arguments:

- `<project-name>` <Required />

  The name of your project.

Running `wasp deploy railway launch` is the same as running the following commands:

```shell
wasp deploy railway setup <project-name>
wasp deploy railway deploy <project-name>
```

<CustomPostgresOption command="launch" />

#### Explicitly providing the Railway project ID

By default, Wasp CLI tries to create a new Railway project named `<project-name>`. If you want to use an existing Railway project, pass its ID with `--existing-project-id` option:

```shell
wasp deploy railway launch <project-name> --existing-project-id <railway-project-id>
```

#### Explicitly providing the Railway Workspace

By default, Wasp CLI will prompt you to select a Railway workspace for your project. If you want to skip the prompt and provide the workspace id or name directly, use the `--workspace` option:

```shell
wasp deploy railway launch <project-name> --workspace <railway-workspace-id-or-name>
```

#### Environment Variables {#railway-launch-environment-variables}

##### Server

If you are deploying an app that requires any other environment variables (like social auth secrets), you can set them with the `--server-secret` option:

```
wasp deploy railway launch my-wasp-app --server-secret GOOGLE_CLIENT_ID=<...> --server-secret GOOGLE_CLIENT_SECRET=<...>
```

##### Client

If you've added any [client-side environment variables](../../../project/env-vars.md#client-env-vars) to your app, pass them to the terminal session before running the `launch` command, for example:

```shell
REACT_APP_ANOTHER_VAR=somevalue wasp deploy railway launch my-wasp-app
```

<CustomServerUrlOption provider="railway" command="launch" example="my-wasp-app" />

### The `deploy` command

The `deploy` command builds your app (server and client) and deploys it to Railway.

```shell
wasp deploy railway deploy <project-name>
```

It accepts the following arguments:

- `<project-name>` <Required />

  The name of your project.

Run this command whenever you want to **update your deployed app** with the latest changes:

```shell
wasp deploy railway deploy <project-name>
```

#### Explicitly providing the Railway project ID

When you run the `deploy` command, Wasp CLI will use the Railway project that's linked to the Wasp project directory. If no Railway project is linked, the command will fail asking you to run the `setup` command first.

If you are deploying your Railway app in the CI, you can pass the `--existing-project-id` option to tell Wasp CLI the Railway project ID to use for the deployment:

```shell
wasp deploy railway deploy <project-name> --existing-project-id <railway-project-id>
```

#### Other Available Options

- `--skip-server` - do not deploy the server
- `--skip-client` - do not deploy the web client. Only applies in [split mode](../../intro.md#deployment-modes); in the default single deployment mode the server serves the client, so the option has no effect and Wasp prints a notice.

If you've added any [client-side environment variables](../../../project/env-vars.md#client-env-vars) to your app, pass them to the terminal session before running the `deploy` command, for example:

```shell
REACT_APP_ANOTHER_VAR=somevalue wasp deploy railway deploy <project-name>
```

You must specify your client-side environment variables every time you redeploy with the above command [to ensure they are included in the build process](../../env-vars.md#client-env-vars).

<CustomServerUrlOption provider="railway" command="deploy" example="my-wasp-app" />

### The `setup` command

The `setup` command creates your server and database services on Railway, plus the client service in [split mode](../../intro.md#deployment-modes). It also configures environment variables. It does _not_ deploy the services.

```shell
wasp deploy railway setup <project-name>
```

It accepts the following arguments:

- `<project-name>`

  the name of your project.

The project name is used as a base for your service names on Railway:

- `<project-name>-server`
- `<project-name>-client`, in split mode only

Railway also creates a PostgreSQL database service named `Postgres`.

<CustomPostgresOption command="setup" />

#### Explicitly providing the Railway project ID

By default, Wasp CLI tries to create a new Railway project named `<project-name>`. If you want to use an existing Railway project, pass its ID with `--existing-project-id` option:

```shell
wasp deploy railway setup <project-name> --existing-project-id <railway-project-id>
```

#### Explicitly providing the Railway Workspace

By default, Wasp CLI will prompt you to select in which Railway workspace you want to create your project. If you want to skip the prompt and provide the workspace id or name directly, use the `--workspace` option:

```shell
wasp deploy railway setup <project-name> --workspace <railway-workspace-id-or-name>
```

:::caution Execute Only Once
You should only run `setup` once per app. Wasp CLI skips creating the services if they already exist.
:::

### Environment Variables {#railway-environment-variables}

#### Server Secrets

If your app requires any other server-side environment variables (like social auth secrets), you can set them:

1. Initially in the `launch` or `setup` commands with the [`--server-secret` option](#railway-launch-environment-variables)
2. After the app has already been deployed, go into the Railway dashboard and set them in the **Variables** tab of your server service.

#### Client Environment Variables

If you've added any [client-side environment variables](../../../project/env-vars.md#client-env-vars) to your app, pass them to the terminal session before running a deployment command. `wasp deploy` builds the client as part of `wasp build`, so the variables must be in its environment. For example:

```shell
REACT_APP_ANOTHER_VAR=somevalue wasp deploy railway launch my-wasp-app
```

or

```shell
REACT_APP_ANOTHER_VAR=somevalue wasp deploy railway deploy
```

Please note that you should do this for **every deployment**, not just the first time you set up the variables. One way to make sure you don't forget to add them is to create a `deploy` script in your `package.json` file:

```json title="package.json"
{
  "scripts": {
    "deploy": "REACT_APP_ANOTHER_VAR=somevalue wasp deploy railway deploy"
  }
}
```

Then you can run `npm run deploy` to deploy your app.
