---
comments: true
---

import AddExternalAuthEnvVarsReminder from '../../deployment/deployment-methods/_addExternalAuthEnvVarsReminder.md'
import { SecretGeneratorBlock } from '../../project/SecretGeneratorBlock'

# Railway

## Deploy Wasp to Railway

This guide shows you how to deploy the client, the server, and provision a database on Railway.

:::info One command deploy

We recommend that you use [Wasp Deploy](../../deployment/deployment-methods/wasp-deploy/railway.md) to deploy your Wasp app to Railway. Wasp CLI automates deploying the client, the server and the database with one command.

:::

### Prerequisites

To get started, follow these steps:

1. Make sure your Wasp app is built by running `wasp build` in the project dir.
1. Create a [Railway](https://railway.com/?utm_medium=integration&utm_source=docs&utm_campaign=wasp) account.
1. Install the [Railway CLI](https://docs.railway.com/develop/cli?utm_medium=integration&utm_source=docs&utm_campaign=wasp#installing-the-cli).
1. Run `railway login` and a browser tab will open to authenticate you.

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

1. Move into the `.wasp/out` directory:

    ```shell
    cd .wasp/out
    ```

2. Link the `.wasp/out` directory to your newly created Railway project:

    ```shell
    railway link
    ```

    Select `server` when prompted to select a service.

3. Go into the Railway dashboard and set up the required env variables:

   Click on the `server` service and go to the **Variables** tab:

   1. Click **Variable reference** and select `DATABASE_URL` (it will populate it with the correct value)

   1. Add `WASP_WEB_CLIENT_URL` with the `client` domain (e.g. `https://client-production-XXXX.up.railway.app`). `https://` prefix is required!

   1. Add `WASP_SERVER_URL` with the `server` domain (e.g. `https://server-production-XXXX.up.railway.app`). `https://` prefix is required!
   1. Add `JWT_SECRET` with a random string at least 32 characters long<br /><SecretGeneratorBlock />

     <AddExternalAuthEnvVarsReminder />

4. Push and deploy the project:

    ```shell
    railway up --ci
    ```

    <small>

    We use the `--ci` flag to limit the log output to only the build process.
    </small>

    Railway will locate the `Dockerfile` in `.wasp/out` and deploy your server.

#### Deploying the Client

1. Create the production build from the project root, using the `server` domain as the `REACT_APP_API_URL`:

    ```shell
    REACT_APP_API_URL=<url_to_wasp_backend> npx vite build
    ```

2. Next, we want to link the client build directory to the `client` service:

    ```shell
    cd .wasp/out/web-app/build
    railway link
    ```

4. Next, deploy the client build to Railway:

    ```shell
    railway up --ci
    ```

    Select `client` when prompted to select a service.

    Railway will detect the `index.html` file and deploy the client as a static site.


And now your Wasp should be deployed!

Back in your [Railway dashboard](https://railway.com/dashboard?utm_medium=integration&utm_source=docs&utm_campaign=wasp), click on your project and you should see your newly deployed services: PostgreSQL, Server, and Client.

### Updates & Redeploying

When you make updates and need to redeploy:

1. Run `wasp build` to rebuild your app.
1. Go into the `.wasp/out` directory and:

    Deploy the server with:
    ```shell
    railway up --ci
    ```
1. Rebuild the client from the project root:
    ```shell
    REACT_APP_API_URL=<url_to_wasp_backend> npx vite build
    ```
    And then deploy the client with:
    ```shell
    cd .wasp/out/web-app/build
    railway up --ci
    ```
