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

The project name is used as a base for your app's service name on Railway:

- `my-wasp-app-server`

<small>
  Your service keeps the `-server` suffix for historical reasons: it used to be
  one of two services, the one serving your API. It now serves your whole app,
  pages included.
</small>

Railway doesn't allow setting the database service name using the Railway CLI. It will always be named `Postgres`. This also applies when using the `--db-image` flag.

:::note Coming From an Older Wasp Version?
Wasp used to deploy your pages as a service of their own, called `my-wasp-app-client`. Your app now serves its own pages, so nothing is deployed to that service anymore.

Once your users are on your app's URL, you can remove it from your Railway project. Wasp reminds you about it when you run `setup` or `deploy`.
:::

<LaunchCommandEnvVars />

If you have any additional environment variables that your app needs, read how to set them in the [API Reference](#railway-environment-variables) section.

<CiCdMention />

## Using a Custom Domain For Your App {#custom-domain}

Setting up a custom domain is a three-step process:

1. Add your domain to your app's Railway service:

    - Go into the [Railway dashboard](https://railway.com/dashboard?utm_medium=integration&utm_source=docs&utm_campaign=wasp).
    - Select your project (for example `my-wasp-app`).
    - Click on your app's service (for example `my-wasp-app-server`).
    - Go to the **Settings** tab and click **Custom Domain**.
    - Enter your domain name (for example `mycoolapp.com`) and port `8080`.
    - Click **Add Domain**.

2. Update the DNS records for your domain, adding a CNAME record at the domain or subdomain you want, pointing to the address you've been given in the previous step. _This step depends on your domain provider, consult their documentation in case of doubt._

3. Tell your app about its new URL, by setting it as the `WASP_SERVER_URL` and `WASP_WEB_CLIENT_URL` environment variables (for example `https://mycoolapp.com`) in the Railway dashboard.

    - Go into the [Railway dashboard](https://railway.com/dashboard?utm_medium=integration&utm_source=docs&utm_campaign=wasp).
    - Select your project (for example `my-wasp-app`).
    - Click on your app's service (for example `my-wasp-app-server`).
    - Go to the **Variables** tab.

    Update both the `WASP_SERVER_URL` and the `WASP_WEB_CLIENT_URL` variables with your new domain.

    <small>
      Wasp builds links from these: the ones in the emails your app sends, and
      the ones it redirects OAuth logins to. `WASP_WEB_CLIENT_URL` defaults to
      `WASP_SERVER_URL`, but `setup` sets both, so you update both.
    </small>

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

Client-side environment variables are part of your app's pages and assets, so they can't be set on the deployed app. Read more about it in the [Client Environment Variables](#client-environment-variables) section.

<CustomServerUrlOption provider="railway" command="launch" example="my-wasp-app" />

### The `deploy` command

The `deploy` command deploys your app to Railway.

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

- `--skip-server` - do not deploy the app
- `--skip-client` - deprecated and ignored, since your app serves its own pages and there is no separate client to deploy

If you've added any [client-side environment variables](../../env-vars.md#client-env-vars) to your app, this command can't get them into your app's pages and assets. Read more about it in the [Client Environment Variables](#client-environment-variables) section.

<CustomServerUrlOption provider="railway" command="deploy" example="my-wasp-app" />

### The `setup` command

The `setup` command creates your app's service and its database service on Railway. It also configures environment variables. It does _not_ deploy your app.

```shell
wasp deploy railway setup <project-name>
```

It accepts the following arguments:

- `<project-name>`

  the name of your project.

The project name is used as a base for your app's service name on Railway:

- `<project-name>-server`

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
2. After the app has already been deployed, go into the Railway dashboard and set them in the **Variables** tab of your app's service.

#### Client Environment Variables

Your [client-side environment variables](../../env-vars.md#client-env-vars) end up inside your app's pages and assets, so they have to be there when your app's image is built, not when it runs. Railway builds that image for you, and `wasp deploy` has no way of passing them to that build yet.

Until it does, if your app needs any `REACT_APP_*` variable, build the image yourself with the `WASP_CLIENT_ENV` build argument and deploy it as described on the [Cloud Providers](../cloud-providers.md) page:

```shell
docker build \
  --build-arg WASP_CLIENT_ENV="REACT_APP_ANOTHER_VAR='somevalue'" \
  -t my-wasp-app \
  .wasp/out
```
