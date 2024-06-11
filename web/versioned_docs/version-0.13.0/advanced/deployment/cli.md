---
title: Deploying with the Wasp CLI
---

import { Required } from '@site/src/components/Tag';

Wasp CLI can deploy your full-stack application with only a single command.
The command automates the manual deployment process and is the recommended way of deploying Wasp apps.

## Supported Providers

Wasp supports automated deployment to the following providers:

- [Fly.io](#flyio) - they offer 5$ free credit each month
- Railway (coming soon, track it here [#1157](https://github.com/wasp-lang/wasp/pull/1157))

## Fly.io

### Prerequisites

Fly provides [free allowances](https://fly.io/docs/about/pricing/#plans) for up to 3 VMs (so deploying a Wasp app to a new account is free), but all plans require you to add your credit card information before you can proceed. If you don't, the deployment will fail.

You can add the required credit card information on the [account's billing page](https://fly.io/dashboard/personal/billing).

:::info Fly.io CLI
You will need the [`flyctl` CLI](https://fly.io/docs/hands-on/install-flyctl/) installed on your machine before you can deploy to Fly.io.
:::

### Deploying

Using the Wasp CLI, you can easily deploy a new app to [Fly.io](https://fly.io) with just a single command:

```shell
wasp deploy fly launch my-wasp-app mia
```

:::caution Specifying Org
If your account is a member of more than one organization on Fly.io, you will need to specify under which one you want to execute the command. To do that, provide an additional `--org <org-slug>` option. You can find out the names(slugs) of your organizations by running `fly orgs list`.
:::

<small>

Please do not CTRL-C or exit your terminal while the commands are running.
</small>

Under the covers, this runs the equivalent of the following commands:

```shell
wasp deploy fly setup my-wasp-app mia
wasp deploy fly create-db mia
wasp deploy fly deploy
```

The commands above use the app basename `my-wasp-app` and deploy it to the _Miami, Florida (US) region_ (called `mia`). Read more about Fly.io regions [here](#flyio-regions).

:::caution Unique Name
Your app name must be unique across all of Fly or deployment will fail.
:::

The basename is used to create all three app tiers, resulting in three separate apps in your Fly dashboard:

- `my-wasp-app-client`
- `my-wasp-app-server`
- `my-wasp-app-db`

You'll notice that Wasp creates two new files in your project root directory:
- `fly-server.toml`
- `fly-client.toml`

You should include these files in your version control so that you can deploy your app with a single command in the future.

### Using a Custom Domain For Your App

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

That's it, your app should be available at `https://mycoolapp.com`! 🎉

## API Reference

### `launch`

`launch` is a convenience command that runs `setup`, `create-db`, and `deploy` in sequence.

```shell
wasp deploy fly launch <app-name> <region>
```

It accepts the following arguments:

- `<app-name>` - the name of your app <Required />
- `<region>` - the region where your app will be deployed <Required />

  Read how to find the available regions [here](#flyio-regions).

It gives you the same result as running the following commands:

```shell
wasp deploy fly setup <app-name> <region>
wasp deploy fly create-db <region>
wasp deploy fly deploy
```

#### Environment Variables

If you are deploying an app that requires any other environment variables (like social auth secrets), you can set them with the `--server-secret` option:

```
wasp deploy fly launch my-wasp-app mia --server-secret GOOGLE_CLIENT_ID=<...> --server-secret GOOGLE_CLIENT_SECRET=<...>
```

### `setup`

`setup` will create your client and server apps on Fly, and add some secrets, but does _not_ deploy them.

```shell
wasp deploy fly setup <app-name> <region>
```

It accepts the following arguments:

- `<app-name>` - the name of your app <Required />
- `<region>` - the region where your app will be deployed <Required />

  Read how to find the available regions [here](#flyio-regions).

After running `setup`, Wasp creates two new files in your project root directory: `fly-server.toml` and `fly-client.toml`.
You should include these files in your version control.

You **can edit the `fly-server.toml` and `fly-client.toml` files** to further configure your Fly deployments. Wasp will use the TOML files when you run `deploy`.

If you want to maintain multiple apps, you can add the `--fly-toml-dir <abs-path>` option to point to different directories, like "dev" or "staging".

:::caution Execute Only Once
You should only run `setup` once per app. If you run it multiple times, it will create unnecessary apps on Fly.
:::

### `create-db`

`create-db` will create a new database for your app.

```shell
wasp deploy fly create-db <region>
```

It accepts the following arguments:

- `<region>` - the region where your app will be deployed <Required />

  Read how to find the available regions [here](#flyio-regions).

:::caution Execute Only Once
You should only run `create-db` once per app. If you run it multiple times, it will create multiple databases, but your app needs only one.
:::

### `deploy`

```shell
wasp deploy fly deploy
```

`deploy` pushes your client and server live.

Run this command whenever you want to **update your deployed app** with the latest changes:

```shell
wasp deploy fly deploy
```

### `cmd`

If want to run arbitrary Fly commands (e.g. `flyctl secrets list` for your server app), here's how to do it:

```shell
wasp deploy fly cmd secrets list --context server
```

### Fly.io Regions

> Fly.io runs applications physically close to users: in datacenters around the world, on servers we run ourselves. You can currently deploy your apps in 34 regions, connected to a global Anycast network that makes sure your users hit our nearest server, whether they’re in Tokyo, São Paolo, or Frankfurt.

<small>

Read more on Fly regions [here](https://fly.io/docs/reference/regions/).
</small>

You can find the list of all available Fly regions by running:

```shell
flyctl platform regions
```

#### Environment Variables

If you are deploying an app that requires any other environment variables (like social auth secrets), you can set them with the `secrets set` command:

```
wasp deploy fly cmd secrets set GOOGLE_CLIENT_ID=<...> GOOGLE_CLIENT_SECRET=<...> --context=server
```

### Multiple Fly Organizations

If you have multiple organizations, you can specify a `--org` option. For example:

```shell
wasp deploy fly launch my-wasp-app mia --org hive
```

### Building Locally

Fly.io offers support for both **locally** built Docker containers and **remotely** built ones. However, for simplicity and reproducibility, the CLI defaults to the use of a remote Fly.io builder.

If you want to build locally, supply the `--build-locally` option to `wasp deploy fly launch` or `wasp deploy fly deploy`.
