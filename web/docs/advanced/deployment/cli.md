---
title: Deploying with the Wasp CLI
---

Wasp CLI supports deploying your full-stack application with a single command. It automates the process a regular user would need to do manually and is the recommended way of deploying Wasp apps.

## Supported Providers

Wasp supports automated deployment to the following providers:
- [Fly.io](#flyio) - they offer 5$ free credit each month
- Railway (coming soon, track it here [#1157](https://github.com/wasp-lang/wasp/pull/1157))

## Fly.io

:::note A Note on Free Tiers 💳
Fly has [free allowances](https://fly.io/docs/about/pricing/#plans) for up to 3 VMs (so deploying a Wasp app to a fresh account is free), but all plans require you to add your credit card info before proceeding. If you don't, the deployment will fail.

To do so, go to your [account's billing page](https://fly.io/dashboard/personal/billing).
:::

Using the Wasp CLI, you can easily deploy a new app to [Fly.io](https://fly.io) with just a single command:
```shell
wasp deploy fly launch my-wasp-app mia
```

Under the covers, this runs the equivalent of the following commands:
```shell
wasp deploy fly setup my-wasp-app mia
wasp deploy fly create-db mia
wasp deploy fly deploy
```

In the above commands, we used an app basename of `my-wasp-app` and deployed it to the Miami, Florida (US) region (called `mia`).

The basename is used to create all three app tiers, so you will have three components in your Fly dashboard:

- `my-wasp-app-client`
- `my-wasp-app-server`
- `my-wasp-app-db`

:::caution Unique Name
Your app name must be unique across all of Fly or deployment will fail. Additionally, please do not CTRL-C or exit your terminal as these commands run.
:::

The list of available Fly regions can be found [here](https://fly.io/docs/reference/regions/). You can also run `flyctl platform regions`.


## Options Reference

### Setup & Create DB

`setup` will create your client and server apps on Fly, and add some secrets, but does _not_ deploy them. We need a database first, which we create with `create-db`, and it is automatically linked to your server.

:::note
We only run the `setup` and `create-db` steps once.
:::

You may notice after running `setup` you have a `fly-server.toml` and `fly-client.toml` in your Wasp project directory. Those are meant to be version controlled.

If you want to maintain multiple apps, you can add the `--fly-toml-dir <abs-path>` option to point to different directories, like "dev" or "staging".

### Deploy

After the setup, we run `deploy` which will push your client and server live.

We run this single command each time you want to **update your app**.  So, to update your app with the latest changes, you run:
```shell
wasp deploy fly deploy
```

:::note
Fly.io offers support for both **locally** built Docker containers and **remotely** built ones. However, for simplicity and reproducibility, the CLI defaults to the use of a remote Fly.io builder.

If you wish to build locally, you may supply the `--build-locally` option to `wasp deploy fly launch` or `wasp deploy fly deploy`.
:::

###  Environment Variables

If you are deploying an app that requires any other environment variables (like social auth secrets), you will want to set your environment variables up like so:

- During `launch`:
  ```
  wasp deploy fly launch my-wasp-app mia --server-secret GOOGLE_CLIENT_ID=<...> --server-secret GOOGLE_CLIENT_SECRET=<...>
  ```

- After `launch`/`setup`:
  ```
  wasp deploy fly cmd secrets set GOOGLE_CLIENT_ID=<...> GOOGLE_CLIENT_SECRET=<...> --context=server
  ```

### Running Fly Commands

If you would like to run arbitrary Fly commands (e.g. `flyctl secrets list` for your server app), you can run them like so:
```shell
wasp deploy fly cmd secrets list --context server
```

### Mutliple Fly Organizations

If you have multiple orgs, you can specify a `--org` option. For example: `wasp deploy fly launch my-wasp-app mia --org hive`

