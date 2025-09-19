---
title: Cloud Providers
---

import useBaseUrl from '@docusaurus/useBaseUrl';
import AddExternalAuthEnvVarsReminder from './\_addExternalAuthEnvVarsReminder.md'
import BuildingTheWebClient from './\_building-the-web-client.md'
import { Required } from '@site/src/components/Tag'
import { Server, Client, Database } from './DeploymentTag'

You can deploy the built Wasp app wherever and however you want, as long as your provider/server supports Wasp's build format.

After going through the general steps that apply to all deployments, you can
follow step-by-step guides for deploying your Wasp app to some of the most popular
providers:

- [Fly.io](#flyio)
- [Railway](#railway)
- [Heroku](#heroku)
- [Netlify](#netlify)
- [Cloudflare](#cloudflare)

No worries, you can still deploy your app if your desired provider isn't on the
list - it just means we don't yet have a step-by-step guide for you to follow.
Feel free to [open a
PR](https://github.com/wasp-lang/wasp/edit/release/web/docs/deployment/deployment-methods/paas.md)
if you'd like to write one yourself :)

## Deploying a Wasp App

Deploying a Wasp app comes down to the following:

1. Generating deployable code.
2. Deploying the API server (backend).
3. Deploying the web client (frontend).
4. Deploying a PostgreSQL database and keeping it running.

Let's go through each of these steps.

### 1. Generating Deployable Code

Running the command `wasp build` generates deployable code for the whole app in the `.wasp/build/` directory.

```
wasp build
```

:::caution PostgreSQL in production
You won't be able to build the app if you are using SQLite as a database (which is the default database).
You'll have to [switch to PostgreSQL](../../data-model/databases.md#migrating-from-sqlite-to-postgresql) before deploying to production.
:::

### 2. Deploying the API Server <Server />

There's a Dockerfile that defines an image for building the server in the `.wasp/build` directory.

To run the server in production, deploy this Docker image to a hosting provider and make sure the required env variables are correctly set up. Usually, you use the provider's dashboard UI or a CLI tool to set up these env variables.

Check the [required server env variables](../env-vars.md#server-env-vars) and make sure they are set up for your server.

While these are the general instructions on deploying the server anywhere, we also have more detailed instructions for chosen providers below, so check that out for more guidance if you are deploying to one of those providers.

### 3. Deploying the Web Client <Client />

<BuildingTheWebClient />

The command above will build the web client and put it in the `build/` directory in the `.wasp/build/web-app/`.

Since the result of building is just a bunch of static files, you can now deploy your web client to any static hosting provider (e.g. Netlify, Cloudflare, ...) by deploying the contents of `.wasp/build/web-app/build/`.

### 4. Deploying the Database <Database />

<!-- TOPIC: database -->

Any PostgreSQL database will do, as long as you provide the server with the correct `DATABASE_URL` env var and ensure that the database is accessible from the server.

## Different Providers

We'll cover a few different deployment providers below:

- Fly.io <Server /> <Database />
- Railway <Server /> <Client /> <Database />
- Heroku <Server /> <Database />
- Netlify <Client />
- Cloudflare <Client />

## Fly.io <Server /> <Database /> {#flyio}


In this section, we'll show how to deploy your server and provision a database for it on Fly.io.

:::info One command deploy

We recommend that you use [Wasp Deploy](./wasp-deploy/fly.md) to deploy your Wasp app to Fly.io. Wasp CLI automates deploying the client, the server and the database with one command.

:::

### Prerequisites

To get started, follow these steps:

1. Create a [Fly.io](https://fly.io/) account,
1. Install the [`fly` CLI](https://fly.io/docs/flyctl/install/),
1. Log in with the `fly` CLI.

You can check if you are logged in with `fly auth whoami`, and if you are not, you can log in with `fly auth login`.

### Set Up a Fly.io App

:::info
You need to do this only once per Wasp app.
:::

Unless you already have a Fly.io app that you want to deploy to, let's create a new Fly.io app.

After you have [built the app](#1-generating-deployable-code), position yourself in `.wasp/build/` directory:

```shell
cd .wasp/build
```

Next, run the launch command to set up a new app and create a `fly.toml` file:

```bash
fly launch --remote-only
```

This will ask you a series of questions, such as asking you to choose a region and whether you'd like a database.

- Say **yes** to **Would you like to set up a PostgreSQL database now?** and select **Development**. Fly.io will set a `DATABASE_URL` for you.
- Say **no** to **Would you like to deploy now?** (and to any additional questions).

  We still need to set up several environment variables.

:::info What if the database setup fails?
If your attempts to initiate a new app fail for whatever reason, then you should run `fly apps destroy <app-name>` before trying again. Fly does not allow you to create multiple apps with the same name.

<details>
  <summary>
    What does it look like when your DB is deployed correctly?
  </summary>

  <div>
    <p>When your DB is deployed correctly, you'll see it in the <a href="https://fly.io/dashboard">Fly.io dashboard</a>:</p>

    <img width="662" alt="image" src="/img/deploying/fly-db.png" />
  </div>
</details>

:::

Next, let's copy the `fly.toml` file up to our Wasp project dir for safekeeping.

```shell
cp fly.toml ../../
```

<!-- TOPIC: env vars -->

Next, add a few more environment variables for the server code.

```bash
fly secrets set PORT=8080
fly secrets set JWT_SECRET=<random_string_at_least_32_characters_long>
fly secrets set WASP_WEB_CLIENT_URL=<url_of_where_client_will_be_deployed>
fly secrets set WASP_SERVER_URL=<url_of_where_server_will_be_deployed>
```

:::note
If you do not know what your client URL is yet, don't worry. You can set `WASP_WEB_CLIENT_URL` after you deploy your client.
:::

<AddExternalAuthEnvVarsReminder />

If you want to make sure you've added your secrets correctly, run `fly secrets list` in the terminal. Note that you will see hashed versions of your secrets to protect your sensitive data.

### Deploy to a Fly.io App

While still in the `.wasp/build/` directory, run:

```bash
fly deploy --remote-only --config ../../fly.toml
```

This will build and deploy the backend of your Wasp app on Fly.io to `https://<app-name>.fly.dev` 🤘🎸

Now, if you haven't, you can deploy your client and add the client URL by running `fly secrets set WASP_WEB_CLIENT_URL=<url_of_deployed_client>`. We suggest using [Netlify](#netlify) for your client, but you can use any static hosting provider.

Additionally, some useful `fly` commands:

```bash
fly logs
fly secrets list
fly ssh console
```

### Redeploying After Wasp Builds

When you rebuild your Wasp app (with `wasp build`), it will remove your `.wasp/build/` directory. In there, you may have a `fly.toml` from any prior Fly.io deployments.

While we will improve this process in the future, in the meantime, you have a few options:

1. Copy the `fly.toml` file to a versioned directory, like your Wasp project dir.

From there, you can reference it in `fly deploy --config <path>` commands, like above.

1. Backup the `fly.toml` file somewhere before running `wasp build`, and copy it into .wasp/build/ after.

When the `fly.toml` file exists in .wasp/build/ dir, you do not need to specify the `--config <path>`.

1. Run `fly config save -a <app-name>` to regenerate the `fly.toml` file from the remote state stored in Fly.io.

## Railway <Server /> <Client /> <Database /> {#railway}


In this section, we'll show how to deploy the client, the server, and provision a database on Railway.

:::info One command deploy

We recommend that you use [Wasp Deploy](./wasp-deploy/railway.md) to deploy your Wasp app to Railway. Wasp CLI automates deploying the client, the server and the database with one command.

:::

### Prerequisites

To get started, follow these steps:

1. Make sure your Wasp app is built by running `wasp build` in the project dir.
1. Create a [Railway](https://railway.com/?utm_medium=integration&utm_source=docs&utm_campaign=wasp) account.
1. Install the [Railway CLI](https://docs.railway.com/develop/cli?utm_medium=integration&utm_source=docs&utm_campaign=wasp#installing-the-cli).
1. Run `railway login` and a browser tab will open to authenticate you.
1. Go to your [Railway account settings](https://railway.com/account/feature-flags?utm_medium=integration&utm_source=docs&utm_campaign=wasp) and enable **Railpack** as the default deployment builder. This is required for the client routing to work correctly.

### Create New Project

Let's create our Railway project:

1. Go to your [Railway dashboard](https://railway.com/dashboard?utm_medium=integration&utm_source=docs&utm_campaign=wasp), click on **New Project**, and select **Deploy PostgreSQL** from the dropdown menu.
1. Once the project is created, left-click on the **Create** button in the top right corner and select **Empty Service**.
1. Click on the new service, and change the name to `server`.
1. Create another empty service and name it `client`.
1. Deploy the changes by pressing the **Deploy** button on top.

### Deploy Your App to Railway

#### Setup Domains

We'll need the domains for both the `server` and `client` services:

1. Go to the `server` instance's **Settings** tab, and click **Generate Domain**.
1. Enter `8080` as the port and click **Generate Domain**.
1. Do the same under the `client`'s **Settings**.
1. Copy both domains, as we will need them later.

#### Deploying the Server

You'll deploy the server first:

1. Move into the `.wasp/build` directory:

    ```shell
    cd .wasp/build
    ```

2. Link the `.wasp/build` directory to your newly created Railway project:

    ```shell
    railway link
    ```

    Select `server` when prompted to select a service.

  <!-- TOPIC: env vars -->

3. Go into the Railway dashboard and set up the required env variables:

   Click on the `server` service and go to the **Variables** tab:

   1. Click **Variable reference** and select `DATABASE_URL` (it will populate it with the correct value)

   1. Add `WASP_WEB_CLIENT_URL` with the `client` domain (e.g. `https://client-production-XXXX.up.railway.app`). `https://` prefix is required!

   1. Add `WASP_SERVER_URL` with the `server` domain (e.g. `https://server-production-XXXX.up.railway.app`). `https://` prefix is required!

   1. Add `JWT_SECRET` with a random string at least 32 characters long (use an [online generator](https://djecrety.ir/))

     <AddExternalAuthEnvVarsReminder />

4. Push and deploy the project:

    ```shell
    railway up --ci
    ```

    <small>

    We use the `--ci` flag to limit the log output to only the build process.
    </small>

    Railway will locate the `Dockerfile` in `.wasp/build` and deploy your server.

#### Deploying the Client

1. Next, go into your app's frontend build directory `.wasp/build/web-app`:

    ```shell
    cd web-app
    ```

2. Create the production build, using the `server` domain as the `REACT_APP_API_URL`:

    ```shell
    npm install && REACT_APP_API_URL=<url_to_wasp_backend> npm run build
    ```

3. Next, we want to link the client build directory to the `client` service:

    ```shell
    cd build
    railway link
    ```

<!-- TOPIC: client deployment -->

4. Next, deploy the client build to Railway:

    ```shell
    railway up --ci
    ```

    Select `client` when prompted to select a service.

    Railway will detect the `index.html` file and deploy the client as a static site using [Railpack](https://railpack.com/languages/staticfile#root-directory-resolution).


And now your Wasp should be deployed!

Back in your [Railway dashboard](https://railway.com/dashboard?utm_medium=integration&utm_source=docs&utm_campaign=wasp), click on your project and you should see your newly deployed services: PostgreSQL, Server, and Client.

### Updates & Redeploying

When you make updates and need to redeploy:

1. Run `wasp build` to rebuild your app.
1. Go into the `.wasp/build` directory and:

    Deploy the server with:
    ```shell
    railway up --ci
    ```
1. Go into the `.wasp/build/web-app` directory and:

    Rebuild the client with:
    ```shell
    npm install && REACT_APP_API_URL=<url_to_wasp_backend> npm run build
    ```
    And then deploy the client with:
    ```shell
    cd build
    railway up --ci
    ```


## Heroku <Server /> <Database /> {#heroku}

We will show how to deploy the server and provision a database for it on Heroku. You can check their [pricing page](https://www.heroku.com/pricing) for more information on their plans.

You will need Heroku account, `heroku` [CLI](https://devcenter.heroku.com/articles/heroku-cli) and `docker` CLI installed to follow these instructions.

Make sure you are logged in with `heroku` CLI. You can check if you are logged in with `heroku whoami`, and if you are not, you can log in with `heroku login`.

### Set up a Heroku app

:::info
You need to do this only once per Wasp app.
:::

Unless you want to deploy to an existing Heroku app, let's create a new Heroku app:

```
heroku create <app-name>
```

Unless you have an external PostgreSQL database that you want to use, let's create a new database on Heroku and attach it to our app:

```
heroku addons:create --app <app-name> heroku-postgresql:essential-0
```

:::caution

We are using the `essential-0` database instance. It's the cheapest database instance Heroku offers and it costs $5/mo.
:::

Heroku will also set `DATABASE_URL` env var for us at this point. If you are using an external database, you will have to set it up yourself.

The `PORT` env var will also be provided by Heroku, so the ones left to set are the `JWT_SECRET`, `WASP_WEB_CLIENT_URL` and `WASP_SERVER_URL` env vars:

<!-- TOPIC: env vars -->

```
heroku config:set --app <app-name> JWT_SECRET=<random_string_at_least_32_characters_long>
heroku config:set --app <app-name> WASP_WEB_CLIENT_URL=<url_of_where_client_will_be_deployed>
heroku config:set --app <app-name> WASP_SERVER_URL=<url_of_where_server_will_be_deployed>
```

:::note
If you do not know what your client URL is yet, don't worry. You can set `WASP_WEB_CLIENT_URL` after you deploy your client.
:::

### Deploy the Heroku app

After you have [built the app](#1-generating-deployable-code), position yourself in `.wasp/build/` directory:

```shell
cd .wasp/build
```

assuming you were at the root of your Wasp project at that moment.

Log in to Heroku Container Registry:

```shell
heroku container:login
```

Set your app's stack to `container` so we can deploy our app as a Docker container:

```shell
heroku stack:set container --app <app-name>
```

Build the Docker image and push it to Heroku:

```shell
heroku container:push --app <app-name> web
```

App is still not deployed at this point.
This step might take some time, especially the very first time, since there are no cached Docker layers.

Deploy the pushed image and restart the app:

```shell
heroku container:release --app <app-name> web
```

This is it, the backend is deployed at `https://<app-name>-XXXX.herokuapp.com` 🎉

Find out the exact app URL with:

```shell
heroku info --app <app-name>
```

Additionally, you can check out the logs with:

```shell
heroku logs --tail --app <app-name>
```

:::note Using `pg-boss` with Heroku

If you wish to deploy an app leveraging [Jobs](../../advanced/jobs) that use `pg-boss` as the executor to Heroku, you need to set an additional environment variable called [`PG_BOSS_NEW_OPTIONS`](../../advanced/jobs.md#pg_boss_new_options) to `{"connectionString":"<REGULAR_HEROKU_DATABASE_URL>","ssl":{"rejectUnauthorized":false}}`. This is because pg-boss uses the `pg` extension, which does not seem to connect to Heroku over SSL by default, which Heroku requires. Additionally, Heroku uses a self-signed cert, so we must handle that as well.

Read more: https://devcenter.heroku.com/articles/connecting-heroku-postgres#connecting-in-node-js
:::

## Netlify <Client /> {#netlify}

Netlify is a static hosting solution that is free for many use cases. You will need a Netlify account to follow these instructions.

Make sure you are logged in with Netlify CLI. You can check if you are logged in with `npx netlify-cli status`, and if you are not, you can log in with `npx netlify-cli login`.

First, make sure you have [built the Wasp app](#1-generating-deployable-code). We'll build the client web app next.

<BuildingTheWebClient />

We can now deploy the client with:

```shell
npx netlify-cli deploy
```

<small>
  Carefully follow the instructions: decide if you want to create a new app or use an existing one, pick the team under which your app will be deployed etc.
</small>

The final step is to run:

```shell
npx netlify-cli deploy --prod
```

That is it! Your client should be live at `https://<app-name>.netlify.app`.

:::note
Make sure you set the `https://<app-name>.netlify.app` URL as the `WASP_WEB_CLIENT_URL` environment variable in your server hosting environment.
:::

:::caution Redirecting URLs toward `index.html`
If you follow the instructions above, the Netlify CLI will use `netlify.toml` file that Wasp generates by default in `.wasp/build/web-app/`. This will correctly configure Netlify to redirect URLs toward `index.html`, which is important since Wasp is a Single Page Application (SPA) and needs to handle routing on the client side.

If you instead use another method of deployment to Netlify, for example doing it using CI, make sure that Netlify picks up that `netlify.toml` file, or configure URL redirecting yourself manually on Netlify.

We recommend to deploy using the Netlify CLI in Github Actions. You can find an example Github Action configuration below.
:::

### Deploying through Github Actions

To enable automatic deployment of the client whenever you push to the `main` branch, you can set up a GitHub Actions workflow. To do this, create a file in your repository at `.github/workflows/deploy.yaml`. Feel free to rename `deploy.yaml` as long as the file type is not changed.

Here’s an example configuration file to help you get started. This example workflow will trigger a deployment to Netlify whenever changes are pushed to the main branch.

<details>
  <summary>Example Github Action</summary>

  ```yaml
  name: Deploy Client to Netlify

  on:
    push:
      branches:
        - main # Deploy on every push to the main branch

  jobs:
    deploy:
      runs-on: ubuntu-latest

      steps:
        - name: Checkout Code
          uses: actions/checkout@v2

        - name: Setup Node.js
          id: setup-node
          uses: actions/setup-node@v4
          with:
            node-version: '20'

        - name: Install Wasp
          run: curl -sSL https://get.wasp.sh/installer.sh | sh -s -- -v 0.16.0 # Change to your Wasp version

        - name: Wasp Build
          run: wasp build

        - name: Install dependencies and build the client
          run: |
            cd ./.wasp/build/web-app
            npm install
            REACT_APP_API_URL=${{ secrets.WASP_SERVER_URL }} npm run build

        - name: Deploy to Netlify
          run: |
            cd ./.wasp/build/web-app
            npx netlify-cli@17.36.1 deploy --prod --dir=build --auth=$NETLIFY_AUTH_TOKEN --site=$NETLIFY_SITE_NAME

      env:
        NETLIFY_AUTH_TOKEN: ${{ secrets.NETLIFY_AUTH_TOKEN }}
        NETLIFY_SITE_NAME: netlify-site-name
  ```
</details>

<details>
  <summary>How do I get the Environment Variables?</summary>

  - **`NETLIFY_AUTH_TOKEN`**: For the auth token, you'll generate a new Personal Access Token on [Netlify](https://docs.netlify.com/cli/get-started/#obtain-a-token-in-the-netlify-ui).

  - **`NETLIFY_SITE_NAME`**: This is the name of your Netlify project.

  - **`WASP_SERVER_URL`**: This is your server's URL and is generally only available after **deploying the backend**. This variable can be skipped when the backend is not functional or not deployed, but be aware that backend-dependent functionalities may be broken.

  After getting the environment variables, you need to set these in GitHub Repository Secrets.
</details>

## Cloudflare <Client /> {#cloudflare}

[Cloudflare](https://www.cloudflare.com/) is a cloud services provider that offers a variety of services, including free static hosting with Cloudflare Pages. You will need a Cloudflare account to follow these instructions.

Make sure you are logged in with the Cloudflare's CLI called Wrangler. You can log in by running:

```bash
npx wrangler login
```

Before you continue, make sure you have [built the Wasp app](#1-generating-deployable-code). We'll build the client web app next.

<BuildingTheWebClient />

To deploy the client, make sure you are positioned in the `.wasp/buld/web-app` folder and then run the following:

```shell
npx wrangler pages deploy ./build --commit-dirty=true --branch=main
```

<small>
  Carefully follow the instructions i.e. do you want to create a new app or use an existing one.
</small>

That is it! Your client should be live at `https://<app-name>.pages.dev`.

:::note
Make sure you set the `https://<app-name>.pages.dev` URL as the `WASP_WEB_CLIENT_URL` environment variable in your server hosting environment.
:::

:::info Redirecting URLs toward `index.html`

Cloudflare will automatically redirect all paths toward `index.html`, which is important since Wasp's client app is a Single Page Application (SPA) and needs to handle routing on the client side.
:::

### Deploying through Github Actions

To enable automatic deployment of the client whenever you push to the `main` branch, you can set up a GitHub Actions workflow. To do this, create a file in your repository at `.github/workflows/deploy.yaml`. Feel free to rename `deploy.yaml` as long as the file type is not changed.

Here’s an example configuration file to help you get started. This example workflow will trigger a deployment to Cloudflare Pages whenever changes are pushed to the main branch.

<details>
  <summary>Example Github Action</summary>

  ```yaml
  name: Deploy Client to Cloudflare

  on:
    push:
      branches:
        - main # Deploy on every push to the main branch

  jobs:
    deploy:
      runs-on: ubuntu-latest

      steps:
        - name: Checkout Code
          uses: actions/checkout@v2

        - name: Setup Node.js
          id: setup-node
          uses: actions/setup-node@v4
          with:
            node-version: '20'

        - name: Install Wasp
          run: curl -sSL https://get.wasp.sh/installer.sh | sh -s -- -v 0.16.0 # Change to your Wasp version

        - name: Wasp Build
          run: cd ./app && wasp build

        - name: Install dependencies and build the client
          run: |
            cd ./app/.wasp/build/web-app
            npm install
            REACT_APP_API_URL=${{ secrets.WASP_SERVER_URL }} npm run build

        - name: Deploy to Cloudflare Pages
          uses: cloudflare/wrangler-action@v3
          with:
            apiToken: ${{ secrets.CLOUDFLARE_API_TOKEN }}
            accountId: ${{ secrets.CLOUDFLARE_ACCOUNT_ID }}
            command: pages deploy ./app/.wasp/build/web-app/build --project-name=${{ env.CLIENT_CLOUDFLARE_APP_NAME }} --commit-dirty=true --branch=main

      env:
        CLIENT_CLOUDFLARE_APP_NAME: cloudflare-pages-app-name
  ```
</details>

<details>
  <summary>How do I get the Environment Variables?</summary>

  - **`CLOUDFLARE_API_TOKEN` and `CLOUDFLARE_ACCOUNT_ID`**: You can get these from your [Cloudflare dashboard](https://dash.cloudflare.com/profile/api-tokens). Make sure to give the token `Cloudflare Pages: Read` and `Cloudflare Pages: Edit` permissions.

  - **`CLIENT_CLOUDFLARE_APP_NAME`**: This is the name of your Cloudflare Pages app. You can create a new Cloudflare Pages app with `npx wrangler pages project create <app-name>`.

  - **`WASP_SERVER_URL`**: This is your server's URL and is generally only available after **deploying the backend**. This variable can be skipped when the backend is not functional or not deployed, but be aware that backend-dependent functionalities may be broken.

  After getting the environment variables, you need to set these in GitHub Repository Secrets.
</details>
