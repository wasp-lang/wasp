---
title: Fly.io
---

import { Required } from '@site/src/components/Tag';
import LaunchCommandEnvVars from './\_launch-command-env-vars.md'
import CiCdMention from './\_ci-cd-mention.md'
import CustomServerUrlOption from './\_custom-server-url-option.md'

[Fly.io](https://fly.io/) is a platform for running containerized apps and microservices on servers around the world. It makes deploying and managing your apps straightforward with minimal setup.

## Prerequisites

To deploy to Fly.io using Wasp CLI:

1. Create a [Fly.io](https://fly.io/) account

1. Fly requires you to add a payment method before you can deploy more than two Fly apps. To deploy a Wasp app, you need two Fly apps: your app and its database.

2. Install the [`fly` CLI](https://fly.io/docs/hands-on/install-flyctl/) on your machine.

## Deploying

Using the Wasp CLI, you can easily deploy a new app to [Fly.io](https://fly.io) with just a single command:

```shell
wasp deploy fly launch my-wasp-app dfw
```

<small>
  Please do not CTRL-C or exit your terminal while the commands are running.
</small>

Two things to keep in mind:

1. Your app name (for example `my-wasp-app`) must be **unique** across all of Fly or deployment will fail.

1. If your account is a member of **more than one organization** on Fly.io, you will need to specify under which one you want to execute the command. To do that, provide an additional `--org <org-slug>` option. You can find out the names (slugs) of your organizations by running `fly orgs list`.

The `launch` command uses the app basename `my-wasp-app` and deploy it to the `dfw` region (`dfw` is short for _Dallas, Texas (US)_). Read more about Fly.io regions [here](#flyio-regions).

The basename is used to name your app and its database, resulting in two apps in your Fly dashboard:

- `my-wasp-app-server`
- `my-wasp-app-db`

<small>
  Your app keeps the `-server` suffix for historical reasons: it used to be one
  of two apps, the one serving your API. It now serves your whole app, pages
  included.
</small>

You'll notice that Wasp creates a new file in your project root directory:

- `fly-server.toml`

You should include this file in your version control so that you can deploy your app with a single command in the future.

:::note Coming From an Older Wasp Version?
Wasp used to deploy your pages as a Fly app of their own, called `my-wasp-app-client`. Your app now serves its own pages, so nothing is deployed to that app anymore.

Once your users are on your app's URL, you can destroy it and delete its TOML file:

```shell
fly apps destroy my-wasp-app-client
rm fly-client.toml
```

Wasp reminds you about this whenever it finds a `fly-client.toml` in your project.
:::

<LaunchCommandEnvVars />

If your app requires any additional environment variables, use the `wasp deploy fly cmd secrets set` command. Read more in the [API Reference](#flyio-cli-environment-variables) section.

<CiCdMention />

## Using a Custom Domain For Your App {#custom-domain}

Setting up a custom domain is a three-step process:

1. You need to add your domain to your Fly app. You can do this by running:

```shell
wasp deploy fly cmd --context server certs create mycoolapp.com
```

:::note Use Your Domain
Make sure to replace `mycoolapp.com` with your domain in all of the commands mentioned in this section.
:::

<small>
  The `server` context is your app. Read more about it in the [`cmd` API
  reference](#cmd).
</small>

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

3. You need to tell your app about its new URL, by setting it as the `WASP_SERVER_URL` and `WASP_WEB_CLIENT_URL` environment variables:

```shell
wasp deploy fly cmd --context server secrets set WASP_SERVER_URL=https://mycoolapp.com WASP_WEB_CLIENT_URL=https://mycoolapp.com
```

<small>
  Wasp builds links from these: the ones in the emails your app sends, and the
  ones it redirects OAuth logins to. `WASP_WEB_CLIENT_URL` defaults to
  `WASP_SERVER_URL`, but `setup` sets both, so you update both.
</small>

That's it, your app should be available at `https://mycoolapp.com`!

### Adding a `www` Subdomain

If you'd also like to access your app at `https://www.mycoolapp.com`, you can generate certificates for the `www` subdomain.

```shell
wasp deploy fly cmd --context server certs create www.mycoolapp.com
```

Once you do that, you will need to add another DNS record for your domain. It should be a CNAME record for `www` with the value of your root domain.
Here's an example:

| Type  | Name | Value         | TTL  |
| ----- | ---- | ------------- | ---- |
| CNAME | www  | mycoolapp.com | 3600 |

With the CNAME record (Canonical name), you are assigning the `www` subdomain as an alias to the root domain.

Your app should now be available both at the root domain `https://mycoolapp.com` and the `www` sub-domain `https://www.mycoolapp.com`.

:::note Pick One of Them for Your Links

Your app serves its pages and its API on the same origin, so serving it on both the `www` and the `non-www` domain needs no extra configuration.

Keep in mind that the links Wasp builds (the ones in your app's emails, and the ones it redirects OAuth logins to) always use `WASP_SERVER_URL`, so they point at whichever of the two domains you set there.

:::

## Environment Variables {#flyio-cli-environment-variables}

### Server Secrets

If your app requires any other server-side environment variables (like social auth secrets), you can set them:

1. Initially, in the `launch` or `setup` commands with the [`--server-secret` option](#fly-launch-environment-variables)
2. After the app has already been deployed by using the `secrets set` command:

    ```
    wasp deploy fly cmd secrets set GOOGLE_CLIENT_ID=<...> GOOGLE_CLIENT_SECRET=<...> --context=server
    ```

### Client Environment Variables

Your [client-side environment variables](../../env-vars.md#client-env-vars) end up inside your app's pages and assets, so they have to be there when your app's image is built, not when it runs. Fly builds that image for you, and `wasp deploy` has no way of passing them to that build yet.

Until it does, if your app needs any `REACT_APP_*` variable, build the image yourself with the `WASP_CLIENT_ENV` build argument and deploy it as described on the [Cloud Providers](../cloud-providers.md) page:

```shell
docker build \
  --build-arg WASP_CLIENT_ENV="REACT_APP_ANOTHER_VAR='somevalue'" \
  -t my-wasp-app \
  .wasp/out
```

## Fly.io Regions

> Fly.io runs applications physically close to users: in datacenters around the world, on servers we run ourselves. You can currently deploy your apps in 34 regions, connected to a global Anycast network that makes sure your users hit our nearest server, whether they’re in Tokyo, São Paolo, or Frankfurt.

<small>
  Read more on Fly regions [here](https://fly.io/docs/reference/regions/).
</small>

You can find the list of all available Fly regions by running:

```shell
fly platform regions
```

## Multiple Fly.io Organizations

If you have multiple organizations, you can specify a `--org` option. For example:

```shell
wasp deploy fly launch my-wasp-app dfw --org hive
```

## Building Locally

Fly.io offers support for both **locally** built Docker containers and **remotely** built ones. However, for simplicity and reproducibility, the CLI defaults to the use of a remote Fly.io builder.

If you want to build locally, supply the `--build-locally` option to `wasp deploy fly launch` or `wasp deploy fly deploy`.

#### Using a custom PostgreSQL database

By default, Wasp uses the standard PostgreSQL Docker image provided by Fly.io when creating a new database for your app. However, if you have a need for a custom Docker image, e.g., your application requires specific PostgreSQL extensions (e.g., PostGIS), you can specify a Docker image with a custom PostgreSQL installation, with the `--db-image <docker-image>` flag.

Your custom PostgreSQL image must be compatible with Fly.io, as their platform has some requirements to work properly. Since these requirements are not readily documented, an easy way to ensure compatibility is to base your custom image off the official Fly.io PostgreSQL image: [`flyio/postgres-flex`](https://hub.docker.com/r/flyio/postgres-flex).

We have crafted a small guide on [how to create a custom Docker image with PostGIS or pgvector for Fly.io](https://gist.github.com/cprecioso/e19e883138241c1a446f48d6187aae75). You can also use it as a starting point to create your own images with other extensions.

:::tip
You only need to specify the Docker image once, when first creating the app with any of these commands:

```shell
wasp deploy fly create-db <region> --db-image <custom-postgres-image>
wasp deploy fly setup <app-name> <region> --db-image <custom-postgres-image>
wasp deploy fly launch <app-name> <region> --db-image <custom-postgres-image>
```
:::


## API Reference

### `launch`

`launch` is a convenience command that runs `setup`, `create-db`, and `deploy` in sequence.

```shell
wasp deploy fly launch <app-name> <region>
```

It accepts the following arguments:

- `<app-name>` <Required />

  The name of your app.

- `<region>`  <Required />

  The region where your app will be deployed. Read how to find the available regions [here](#flyio-regions).

Running `wasp deploy fly launch` is the same as running the following commands:

```shell
wasp deploy fly setup <app-name> <region>
wasp deploy fly create-db <region>
wasp deploy fly deploy
```

#### Environment Variables {#fly-launch-environment-variables}

##### Server

If you are deploying an app that requires any other environment variables (like social auth secrets), you can set them with the `--server-secret` option:

```
wasp deploy fly launch my-wasp-app dfw --server-secret GOOGLE_CLIENT_ID=<...> --server-secret GOOGLE_CLIENT_SECRET=<...>
```

##### Client

Client-side environment variables are part of your app's pages and assets, so they can't be set on the deployed app. Read more about it in the [Client Environment Variables](#client-environment-variables) section.

<CustomServerUrlOption provider="fly" command="launch" example="my-wasp-app dfw" />

### `setup`

The `setup` command registers your app on Fly, and sets up needed environment variables.
It only needs to be run once, when initially creating the app. It does _not_ trigger a deploy.

```shell
wasp deploy fly setup <app-name> <region>
```

It accepts the following arguments:

- `<app-name>` <Required />

  The name of your app.

- `<region>` <Required />

  The region where your app will be deployed. Read how to find the available regions [here](#flyio-regions).

After running `setup`, Wasp creates a new file in your project root directory: `fly-server.toml`.
You should include this file in your version control.

You **can edit the `fly-server.toml` file** to further configure your Fly deployment. Wasp will use the TOML file when you run `deploy`.

If you want to maintain multiple apps, you can add the `--fly-toml-dir <abs-path>` option to point to different directories, like "dev" or "staging".

:::caution Execute Only Once
You should only run `setup` once per app. Wasp skips it when it finds a `fly-server.toml` file, but if that file is missing, running `setup` again creates another app on Fly.
:::

### `create-db`

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

### `deploy`

```shell
wasp deploy fly deploy
```

The `deploy` command pushes your built app live.

Run this command whenever you want to **update your deployed app** with the latest changes:

```shell
wasp deploy fly deploy
```

If you've added any [client-side environment variables](../../env-vars.md#client-env-vars) to your app, this command can't get them into your app's pages and assets. Read more about it in the [Client Environment Variables](#client-environment-variables) section.

<CustomServerUrlOption provider="fly" command="deploy" example="" />

### `cmd`

If you want to run arbitrary Fly commands (for example `fly secrets list` for your app), here's how to do it:

```shell
wasp deploy fly cmd secrets list --context server
```

The `server` context is your app, named that way back when your pages and your API were two separate Fly apps. If you still have the client app from such an older project, `--context client` runs the command against it.
