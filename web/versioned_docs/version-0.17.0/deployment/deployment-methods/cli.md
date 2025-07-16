---
title: Wasp CLI
---

import { Required } from '@site/src/components/Tag';
import LaunchCommandEnvVars from './\_launch-command-env-vars.md'


Wasp CLI can deploy your full-stack application with only a single command.
The command automates the manual deployment process and is the recommended way of deploying Wasp apps.

## Supported Providers

Wasp supports automated deployment to the following providers:

- [Fly.io](#flyio)
- [Railway](#railway)

## Fly.io {#flyio}

### Prerequisites

Fly requires you to add your credit card information before you can deploy your apps. If you don't, the deployment will fail. Prices are usage based and can be [estimated here](https://fly.io/calculator).

You can add the required credit card information on the [account's billing page](https://fly.io/dashboard/personal/billing).

:::info Fly.io CLI
You will need the [`fly` CLI](https://fly.io/docs/hands-on/install-flyctl/) installed on your machine before you can deploy to Fly.io.
:::

### Deploying

Using the Wasp CLI, you can easily deploy a new app to [Fly.io](https://fly.io) with just a single command:

```shell
wasp deploy fly launch my-wasp-app mia
```

<small>
  Please do not CTRL-C or exit your terminal while the commands are running.
</small>

Two things to keep in mind:

1. Your app name (for example `my-wasp-app`) must be **unique** across all of Fly or deployment will fail.

1. If your account is a member of **more than one organization** on Fly.io, you will need to specify under which one you want to execute the command. To do that, provide an additional `--org <org-slug>` option. You can find out the names (slugs) of your organizations by running `fly orgs list`.

The `launch` command uses the app basename `my-wasp-app` and deploy it to the `mia` region (`mia` is short for _Miami, Florida (US)_). Read more about Fly.io regions [here](#flyio-regions).

The basename is used to create all three app tiers, resulting in three separate apps in your Fly dashboard:

- `my-wasp-app-client`
- `my-wasp-app-server`
- `my-wasp-app-db`

You'll notice that Wasp creates two new files in your project root directory:

- `fly-server.toml`
- `fly-client.toml`

You should include these files in your version control so that you can deploy your app with a single command in the future.

<LaunchCommandEnvVars />

If your app requires any additional environment variables, use the `wasp deploy fly cmd secrets set` command. Read more in the [API Reference](#flyio-cli-environment-variables) section.

### Using a Custom Domain For Your App {#fly-custom-domain}

Setting up a custom domain is a three-step process:

1. You need to add your domain to your Fly client app. You can do this by running:

```shell
wasp deploy fly cmd --context client certs create mycoolapp.com
```

:::note Use Your Domain
Make sure to replace `mycoolapp.com` with your domain in all of the commands mentioned in this section.
:::

This command will output the instructions to add the DNS records to your domain. It will look something like this:

```shell-session
You can direct traffic to mycoolapp.com by:

1: Adding an A record to your DNS service which reads

    A @ 66.241.1XX.154

You can validate your ownership of mycoolapp.com by:

2: Adding an AAAA record to your DNS service which reads:

    AAAA @ 2a09:82XX:1::1:ff40
```

2. You need to add the DNS records for your domain:

   _This will depend on your domain provider, but it should be a matter of adding an A record for `@` and an AAAA record for `@` with the values provided by the previous command._

3. You need to set your domain as the `WASP_WEB_CLIENT_URL` environment variable for your server app:

```shell
wasp deploy fly cmd --context server secrets set WASP_WEB_CLIENT_URL=https://mycoolapp.com
```

<small>
  We need to do this to keep our CORS configuration up to date.
</small>

That's it, your app should be available at `https://mycoolapp.com`!

#### Adding a `www` Subdomain

If you'd also like to access your app at `https://www.mycoolapp.com`, you can generate certificates for the `www` subdomain.

```shell
wasp deploy fly cmd --context client certs create www.mycoolapp.com
```

Once you do that, you will need to add another DNS record for your domain. It should be a CNAME record for `www` with the value of your root domain.
Here's an example:

| Type  | Name | Value         | TTL  |
| ----- | ---- | ------------- | ---- |
| CNAME | www  | mycoolapp.com | 3600 |

With the CNAME record (Canonical name), you are assigning the `www` subdomain as an alias to the root domain.

Your app should now be available both at the root domain `https://mycoolapp.com` and the `www` sub-domain `https://www.mycoolapp.com`.

:::caution CORS Configuration

Using the `www` and `non-www` domains at the same time will require you to update your CORS configuration to allow both domains. You'll need to provide [custom CORS configuration](https://gist.github.com/infomiho/5ca98e5e2161df4ea78f76fc858d3ca2) in your server app to allow requests from both domains.

:::

## Railway {#railway}

### Prerequisites

To deploy to Railway using Wasp CLI:

1. Create a [Railway](https://railway.com/) account,

1. Wasp CLI requires that Railpack is set as the default deployment builder **for client routing to work correctly**. Go to your [Railway account settings](https://railway.com/account/feature-flags) and enable "Default to Railpack".

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

1. Your project name (for example `my-wasp-app`) must be unique across all your Railway projects or deployment will fail (his is a current limitation of the Wasp CLI and Railway integration [#2926](https://github.com/wasp-lang/wasp/issues/2926)).

1. If you are a member of multiple Railway organizations, the CLI will prompt you to select the organization under which you want to deploy your app.

The project name is used as a base for your server and client service names on Railway:

- `my-wasp-app-client`
- `my-wasp-app-server`

Railway doesn't allow setting the database service name using the Railway CLI. It will always be named `Postgres`.

<LaunchCommandEnvVars />

If you have any additional environment variables that your app needs, read how to set them in the [API Reference](#railway-environment-variables) section.

### Using a Custom Domain For Your App {#railway-custom-domain}

Setting up a custom domain is a three-step process:

1. Add your domain to the Railway client service:

    - Go into the [Railway dashboard](https://railway.app/dashboard).
    - Select your project (for example `my-wasp-app`).
    - Click on the client service (for example `my-wasp-app-client`).
    - Go to the **Settings** tab and click **Custom Domain**.
    - Enter your domain name (for example `mycoolapp.com`) and port `8080`.
    - Click **Add Domain**.

2. Update the DNS records for your domain, adding a CNAME record at the domain or subdomain you want, pointing to the address you've been given in the previous step. _This step depends on your domain provider, consult their documention in case of doubt._

3. To avoid [CORS](https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/CORS) errors, you need to set your new client URL as the `WASP_WEB_CLIENT_URL` environment variable (for example `https://mycoolapp.com`) for your **server service** in the Railway dashboard.

    - Go into the [Railway dashboard](https://railway.app/dashboard).
    - Select your project (for example `my-wasp-app`).
    - Click on the server service (for example `my-wasp-app-server`).
    - Go to the **Variables** tab.

    Update the `WASP_WEB_CLIENT_URL` variable with the new domain for your client.

That's it, your app should be available at `https://mycoolapp.com`!

## API Reference

### Fly.io CLI Commands {#flyio-cli}

#### The `launch` command

`launch` is a convenience command that runs `setup`, `create-db`, and `deploy` in sequence.

```shell
wasp deploy fly launch <app-name> <region>
```

It accepts the following arguments:

- `<app-name>` <Required />

  The name of your app.

- `<region>`  <Required />

  The region where your app will be deployed. Read how to find the available regions [here](#flyio-regions).

Running `wasp deploy fly launch` is same as running the following commands:

```shell
wasp deploy fly setup <app-name> <region>
wasp deploy fly create-db <region>
wasp deploy fly deploy
```

##### Environment Variables {#fly-launch-environment-variables}

###### Server

If you are deploying an app that requires any other environment variables (like social auth secrets), you can set them with the `--server-secret` option:

```
wasp deploy fly launch my-wasp-app mia --server-secret GOOGLE_CLIENT_ID=<...> --server-secret GOOGLE_CLIENT_SECRET=<...>
```

###### Client

If you've added any [client-side environment variables](../../project/env-vars#client-env-vars) to your app, pass them to the terminal session before running the `launch` command, for example:

```shell
REACT_APP_ANOTHER_VAR=somevalue wasp deploy fly launch my-wasp-app mia
```

#### The `setup` command

The `setup` command registers your client and server apps on Fly, and sets up needed environment variables.
It only needs to be run once, when initially creating the app. It does _not_ trigger a deploy for the client or server apps.

```shell
wasp deploy fly setup <app-name> <region>
```

It accepts the following arguments:

- `<app-name>` <Required />

  The name of your app.

- `<region>` <Required />

  The region where your app will be deployed. Read how to find the available regions [here](#flyio-regions).

After running `setup`, Wasp creates two new files in your project root directory: `fly-server.toml` and `fly-client.toml`.
You should include these files in your version control.

You **can edit the `fly-server.toml` and `fly-client.toml` files** to further configure your Fly deployments. Wasp will use the TOML files when you run `deploy`.

If you want to maintain multiple apps, you can add the `--fly-toml-dir <abs-path>` option to point to different directories, like "dev" or "staging".

:::caution Execute Only Once
You should only run `setup` once per app. If you run it multiple times, it creates unnecessary apps on Fly.
:::

#### The `create-db` command

The `create-db` command creates a new database for your app.

```shell
wasp deploy fly create-db <region>
```

It accepts the following arguments:

- `<region>` <Required />

  The region where your app will be deployed. Read how to find the available regions [here](#flyio-regions).

:::caution Execute Only Once
You should only run `create-db` once per app. If you run it multiple times, it creates multiple databases, but your app needs only one.
:::

#### The `deploy` command

```shell
wasp deploy fly deploy
```

The `deploy` command pushes your built client and server live.

Run this command whenever you want to **update your deployed app** with the latest changes:

```shell
wasp deploy fly deploy
```

If you've added any [client-side environment variables](../../project/env-vars#client-env-vars) to your app, pass them to the terminal session before running the `deploy` command, for example:

```shell
REACT_APP_ANOTHER_VAR=somevalue wasp deploy fly deploy
```

You must specify your client-side environment variables every time you redeploy with the above command [to ensure they are included in the build process](../../project/env-vars#client-env-vars-1).

#### The `cmd` command

If you want to run arbitrary Fly commands (for example `fly secrets list` for your server app), here's how to do it:

```shell
wasp deploy fly cmd secrets list --context server
```

#### Environment Variables {#flyio-cli-environment-variables}

##### Server Secrets

If your app requires any other server-side environment variables (like social auth secrets), you can set them:

1. Initially in the `launch` or `setup` commands with the [`--server-secret` option](#fly-launch-environment-variables)
2. After the app has already been deployed by using the `secrets set` command:

    ```
    wasp deploy fly cmd secrets set GOOGLE_CLIENT_ID=<...> GOOGLE_CLIENT_SECRET=<...> --context=server
    ```

##### Client Environment Variables

If you've added any [client-side environment variables](../../project/env-vars#client-env-vars) to your app, pass them to the terminal session before running a deployment command, for example:

```shell
REACT_APP_ANOTHER_VAR=somevalue wasp deploy fly launch my-wasp-app mia
```

or

```shell
REACT_APP_ANOTHER_VAR=somevalue wasp deploy fly deploy
```

Please note that you should do this for **every deployment**, not just the first time you set up the variables. One way to make sure you don't forget to add them is to create a `deploy` script in your `package.json` file:

```json title="package.json"
{
  "scripts": {
    "deploy": "REACT_APP_ANOTHER_VAR=somevalue wasp deploy fly deploy"
  }
}
```

Then you can run `npm run deploy` to deploy your app.

#### Fly.io Regions

> Fly.io runs applications physically close to users: in datacenters around the world, on servers we run ourselves. You can currently deploy your apps in 34 regions, connected to a global Anycast network that makes sure your users hit our nearest server, whether they’re in Tokyo, São Paolo, or Frankfurt.

<small>
  Read more on Fly regions [here](https://fly.io/docs/reference/regions/).
</small>

You can find the list of all available Fly regions by running:

```shell
fly platform regions
```

#### Multiple Fly.io Organizations

If you have multiple organizations, you can specify a `--org` option. For example:

```shell
wasp deploy fly launch my-wasp-app mia --org hive
```

#### Building Locally

Fly.io offers support for both **locally** built Docker containers and **remotely** built ones. However, for simplicity and reproducibility, the CLI defaults to the use of a remote Fly.io builder.

If you want to build locally, supply the `--build-locally` option to `wasp deploy fly launch` or `wasp deploy fly deploy`.

### Railway CLI Commands {#railway-cli}

#### The `launch` command

`launch` is a convenience command that runs `setup` and `deploy` in sequence.

```shell
wasp deploy railway launch <project-name>
```

It accepts the following arguments:

- `<project-name>` <Required />

  The name of your project.

Running `wasp deploy railway launch` is same as running the following commands:

```shell
wasp deploy railway setup <project-name>
wasp deploy railway deploy <project-name>
```

##### Explicitly providing the Railway project ID

By default, Wasp CLI tries to create a new Railway project named `<project-name>`. If you want to use an existing Railway project, pass its ID with `--existing-project-id` option:

```shell
wasp deploy railway launch <project-name> --existing-project-id <railway-project-id>
```

##### Environment Variables {#railway-launch-environment-variables}

###### Server

If you are deploying an app that requires any other environment variables (like social auth secrets), you can set them with the `--server-secret` option:

```
wasp deploy railway launch my-wasp-app --server-secret GOOGLE_CLIENT_ID=<...> --server-secret GOOGLE_CLIENT_SECRET=<...>
```

###### Client

If you've added any [client-side environment variables](../../project/env-vars#client-env-vars) to your app, pass them to the terminal session before running the `launch` command, for example:

```shell
REACT_APP_ANOTHER_VAR=somevalue wasp deploy railway launch my-wasp-app
```

#### The `deploy` command

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

##### Explicitly providing the Railway project ID

When you run the `deploy` command, Wasp CLI will use the Railway project that's linked to the Wasp project directory. If no Railway project is linked, the command will fail asking you to run the `setup` command first.

If you are deploying your Railway app in the CI, you can pass the `--existing-project-id` option to tell Wasp CLI the Railway project ID to use for the deployment:

```shell
wasp deploy railway deploy <project-name> --existing-project-id <railway-project-id>
```

##### Other Available Options

- `--skip-client` - do not deploy the web client
- `--skip-server` - do not deploy the server

If you've added any [client-side environment variables](../../project/env-vars#client-env-vars) to your app, pass them to the terminal session before running the `deploy` command, for example:

```shell
REACT_APP_ANOTHER_VAR=somevalue wasp deploy railway deploy <project-name>
```

You must specify your client-side environment variables every time you redeploy with the above command [to ensure they are included in the build process](../../project/env-vars#client-env-vars-1).

#### The `setup` command

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

##### Explicitly providing the Railway project ID

By default, Wasp CLI tries to create a new Railway project named `<project-name>`. If you want to use an existing Railway project, pass its ID with `--existing-project-id` option:

```shell
wasp deploy railway setup <project-name> --existing-project-id <railway-project-id>
```

:::caution Execute Only Once
You should only run `setup` once per app. Wasp CLI skips creating the services if they already exist.
:::

#### Environment Variables {#railway-environment-variables}

##### Server Secrets

If your app requires any other server-side environment variables (like social auth secrets), you can set them:

1. Initially in the `launch` or `setup` commands with the [`--server-secret` option](#railway-launch-environment-variables)
2. After the app has already been deployed go into the Railway dashboard and set them in the **Variables** tab of your server service.

##### Client Environment Variables

If you've added any [client-side environment variables](../../project/env-vars#client-env-vars) to your app, pass them to the terminal session before running a deployment command, for example:

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
