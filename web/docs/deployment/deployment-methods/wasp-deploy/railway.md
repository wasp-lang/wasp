---
title: Railway
title-llm: Automated Deployment to Railway with Wasp CLI
---

import { Required } from '@site/src/components/Tag';
import LaunchCommandEnvVars from './\_launch-command-env-vars.md'

[Railway](https://railway.com/?utm_medium=integration&utm_source=docs&utm_campaign=wasp) is a cloud development platform that streamlines building and deploying applications with built-in support for databases and services. It offers an intuitive interface and automates infrastructure.

## Prerequisites

To deploy to Railway using Wasp CLI:

1. Create a [Railway](https://railway.com/?utm_medium=integration&utm_source=docs&utm_campaign=wasp) account,

1. Wasp CLI requires that Railpack is set as the default deployment builder **for client routing to work correctly**. Go to your [Railway account settings](https://railway.com/account/feature-flags?utm_medium=integration&utm_source=docs&utm_campaign=wasp) and enable "Default to Railpack".

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

The project name is used as a base for your server and client service names on Railway:

- `my-wasp-app-client`
- `my-wasp-app-server`

Railway doesn't allow setting the database service name using the Railway CLI. It will always be named `Postgres`.

<LaunchCommandEnvVars />

If you have any additional environment variables that your app needs, read how to set them in the [API Reference](#railway-environment-variables) section.

## Using a Custom Domain For Your App {#custom-domain}

Setting up a custom domain is a three-step process:

1. Add your domain to the Railway client service:

    - Go into the [Railway dashboard](https://railway.com/dashboard?utm_medium=integration&utm_source=docs&utm_campaign=wasp).
    - Select your project (for example `my-wasp-app`).
    - Click on the client service (for example `my-wasp-app-client`).
    - Go to the **Settings** tab and click **Custom Domain**.
    - Enter your domain name (for example `mycoolapp.com`) and port `8080`.
    - Click **Add Domain**.

2. Update the DNS records for your domain, adding a CNAME record at the domain or subdomain you want, pointing to the address you've been given in the previous step. _This step depends on your domain provider, consult their documentation in case of doubt._

3. To avoid [CORS](https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/CORS) errors, you need to set your new client URL as the `WASP_WEB_CLIENT_URL` environment variable (for example `https://mycoolapp.com`) for your **server service** in the Railway dashboard.

    - Go into the [Railway dashboard](https://railway.com/dashboard?utm_medium=integration&utm_source=docs&utm_campaign=wasp).
    - Select your project (for example `my-wasp-app`).
    - Click on the server service (for example `my-wasp-app-server`).
    - Go to the **Variables** tab.

    Update the `WASP_WEB_CLIENT_URL` variable with the new domain for your client.

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

#### Explicitly providing the Railway project ID

By default, Wasp CLI tries to create a new Railway project named `<project-name>`. If you want to use an existing Railway project, pass its ID with `--existing-project-id` option:

```shell
wasp deploy railway launch <project-name> --existing-project-id <railway-project-id>
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

### The `deploy` command

The `deploy` command deploys your client and server apps to Railway.

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

- `--skip-client` - do not deploy the web client
- `--skip-server` - do not deploy the server

If you've added any [client-side environment variables](../../../project/env-vars.md#client-env-vars) to your app, pass them to the terminal session before running the `deploy` command, for example:

```shell
REACT_APP_ANOTHER_VAR=somevalue wasp deploy railway deploy <project-name>
```

You must specify your client-side environment variables every time you redeploy with the above command [to ensure they are included in the build process](../../env-vars.md#client-env-vars).

### The `setup` command

The `setup` command creates your client, server, and database services on Railway. It also configures environment variables. It does _not_ deploy the client or server services.

```shell
wasp deploy railway setup <project-name>
```

It accepts the following arguments:

- `<project-name>` 

  the name of your project.

The project name is used as a base for your server and client service names on Railway:

- `<project-name>-client`
- `<project-name>-server`

Railway also creates a PostgreSQL database service named `Postgres`.

#### Explicitly providing the Railway project ID

By default, Wasp CLI tries to create a new Railway project named `<project-name>`. If you want to use an existing Railway project, pass its ID with `--existing-project-id` option:

```shell
wasp deploy railway setup <project-name> --existing-project-id <railway-project-id>
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

If you've added any [client-side environment variables](../../../project/env-vars.md#client-env-vars) to your app, pass them to the terminal session before running a deployment command, for example:

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
