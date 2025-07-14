---
title: Railway
---

import { Required } from '@site/src/components/Tag';

### Prerequisites

:::caution Railpack required

Wasp CLI requires that Railpack is set as the default deployment builder **for client routing to work correctly**.

Go to your [Railway account settings](https://railway.com/account/feature-flags) and enable "Default to Railpack".

:::

To deploy to Railway using Wasp CLI, make sure to:

1. Create a [Railway](https://railway.com/) account,

1. Install the [`railway` CLI](https://docs.railway.com/guides/cli#installing-the-cli) on your machine.

### Deploying

Using the Wasp CLI, you can easily deploy a new app to Railway with a single command:

```shell
wasp deploy railway launch my-wasp-app
```

<small>
  Please do not CTRL-C or exit your terminal while the commands are running.
</small>

Keep in mind that:

1. Your project name (for example `my-wasp-app`) must be unique across all your Railway projects or deployment will fail. This is a current limitation of the Wasp CLI and Railway integration ([#2926](https://github.com/wasp-lang/wasp/issues/2926)).

1. If you are a member of multiple Railway organizations, the CLI will prompt you to select the organization under which you want to deploy your app.

Under the hood, `wasp deploy railway launch` is the same as running:

```shell
wasp deploy railway setup my-wasp-app
wasp deploy railway deploy my-wasp-app
```

The project name is used as a base for your server and client service names on Railway:

- `my-wasp-app-client`
- `my-wasp-app-server`

Railway doesn't allow setting the database service name using the Railway CLI, so it will always be named `Postgres`.

When you run the `launch` command, Wasp CLI sets some [required environment variables](../../project/env-vars.md#wasp-server-env-vars) in the server app automatically.

If you have any additional environment variables that your app needs, read how to set them in the [API Reference](#railway-environment-variables).

### Using a Custom Domain For Your App {#railway-custom-domain}

Setting up a custom domain is a three-step process:

1. You need to add your domain to your Railway client service:

    - Go into the [Railway dashboard](https://railway.app/dashboard).
    - Select your project (for example `my-wasp-app`).
    - Click on the client service (for example `my-wasp-app-client`).
    - Go to the `Settings` tab and click `Custom Domain`.
    - Enter your domain name (for example `mycoolapp.com`) and port `8080`.
    - Click `Add Domain`.

2. You need to add the DNS records for your domain:

   _This will depend on your domain provider, but it should be a matter of adding a CNAME record with the values provided by Railway._

3. You need to set your new client URL as the `WASP_WEB_CLIENT_URL` environment variable (for example `https://mycoolapp.com`) for your **server service** in the Railway dashboard.

    - Go into the [Railway dashboard](https://railway.app/dashboard).
    - Select your project (for example `my-wasp-app`).
    - Click on the server service (for example `my-wasp-app-server`).
    - Go to the `Variables` tab.

    We need to update the `WASP_WEB_CLIENT_URL` env variable to keep our CORS configuration up to date.

That's it, your app should be available at `https://mycoolapp.com`!

## API Reference

### `launch` command

`launch` is a convenience command that runs `setup` and `deploy` in sequence.

```shell
wasp deploy railway launch <project-name>
```

It accepts the following arguments:

- `<project-name>` - the name of your project <Required />

It gives you the same result as running the following commands:

```shell
wasp deploy railway setup <project-name>
wasp deploy railway deploy <project-name>
```

#### Explicitly providing the Railway project ID

Wasp CLI tries to create a new Railway project named `<project-name>`, but you can use an existing project you have created on Railway by passing the `--existing-project-id [projectId]` option.

#### Environment Variables {#railway-launch-environment-variables}

##### Server

If you are deploying an app that requires any other environment variables (like social auth secrets), you can set them with the `--server-secret` option:

```
wasp deploy railway launch my-wasp-app --server-secret GOOGLE_CLIENT_ID=<...> --server-secret GOOGLE_CLIENT_SECRET=<...>
```

##### Client

If you've added any [client-side environment variables](../../project/env-vars#client-env-vars) to your app, make sure to pass them to the terminal session before running the `launch` command, for example:

```shell
REACT_APP_ANOTHER_VAR=somevalue wasp deploy railway launch my-wasp-app
```

### `deploy` command

The `deploy` command deploys your client and server apps to Railway.

```shell
wasp deploy railway deploy <project-name>
```

It accepts the following arguments:

- `<project-name>` - the name of your project <Required />

Run this command whenever you want to **update your deployed app** with the latest changes:

```shell
wasp deploy railway deploy my-wasp-app
```

#### Explicitly providing the Railway project ID

When you run the `deploy` command, Wasp CLI will use the Railway project that's linked to the Wasp project directory. If no Railway project is linked, the command will fail asking you to run the `setup` command first.

If you are deploying your Railway app in the CI, you can pass the `--existing-project-id [projectId]` option to tell Wasp CLI the Railway project ID to use for the deployment.

#### Other Available Options

- `--skip-client` - do not deploy the web client
- `--skip-server` - do not deploy the server

If you've added any [client-side environment variables](../../project/env-vars#client-env-vars) to your app, make sure to pass them to the terminal session before running the `deploy` command, for example:

```shell
REACT_APP_ANOTHER_VAR=somevalue wasp deploy railway deploy my-wasp-app
```

Make sure to add your client-side environment variables every time you redeploy with the above command [to ensure they are included in the build process](../../project/env-vars#client-env-vars-1).

### `setup` command

`setup` will create your client, server, and database services on Railway, and configure environment variables. It does _not_ deploy the client or server services.

```shell
wasp deploy railway setup <project-name>
```

It accepts the following arguments:

- `<project-name>` - the name of your project <Required />

The project name is used as a base for your server and client service names on Railway:

- `<project-name>-client`
- `<project-name>-server`

Railway will also create a PostgreSQL database service named `Postgres`.

#### Explicitly providing the Railway project ID

You can use an existing project you have created on Railway by passing the `--existing-project-id [projectId]` option.

:::caution Execute Only Once
You should only run `setup` once per app. Wasp CLI will skip creating the services if they already exist.
:::

### Environment Variables {#railway-environment-variables}

#### Server Secrets

If your app requires any other server-side environment variables (like social auth secrets), you can set them:

1. initially in the `launch` or `setup` commands with the [`--server-secret` option](#railway-launch-environment-variables),\
   or
2. after the app has already been deployed go into the Railway dashboard and set them in the `Variables` tab of your server service.

#### Client Environment Variables

If you've added any [client-side environment variables](../../project/env-vars#client-env-vars) to your app, make sure to pass them to the terminal session before running a deployment command, for example:

```shell
REACT_APP_ANOTHER_VAR=somevalue wasp deploy railway launch my-wasp-app
```

or

```shell
REACT_APP_ANOTHER_VAR=somevalue wasp deploy railway deploy
```

Please note, that you should do this for **every deployment**, not just the first time you set up the variables. One way to make sure you don't forget to add them is to create a `deploy` script in your `package.json` file:

```json title="package.json"
{
  "scripts": {
    "deploy": "REACT_APP_ANOTHER_VAR=somevalue wasp deploy railway deploy"
  }
}
```

Then you can run `npm run deploy` to deploy your app.
