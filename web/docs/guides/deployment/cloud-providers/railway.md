---
comments: true
---

import LastCheckedWithVersionsNotice from "@site/src/components/LastCheckedWithVersionsNotice";
import AddExternalAuthEnvVarsReminder from './_addExternalAuthEnvVarsReminder.md'
import { SecretGeneratorBlock } from '../../../project/SecretGeneratorBlock'
import { Server, Client, Database } from '../DeploymentTag'

# Railway

<LastCheckedWithVersionsNotice versions={{ Wasp: "0.26", Railway: new Date("2026-04-06") }} />

## Automatic Deployment <Server /> <Client /> <Database />

We recommend that you use [Wasp Deploy](../../../deployment/deployment-methods/wasp-deploy/railway.md) to deploy your Wasp app to Railway. Wasp CLI automates deploying your app and its database with one command.

## Manual Deployment <Server /> <Client /> <Database />

This guide shows you how to deploy your app and provision a database on Railway.

A built Wasp app is a single server that serves your app's pages, its static assets and its API, so it needs a single Railway service.

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
1. Click on the new service, and change the name to `app`.
1. Deploy the changes by pressing the **Deploy** button on top.

### Deploy Your App to Railway

#### Setup the Domain

We'll need the domain for the `app` service:

1. Go to the `app` instance's **Settings** tab, and click **Generate Domain**.
1. Enter `8080` as the port and click **Generate Domain**.
1. Copy the domain, as we will need it later.

#### Deploying the App

1. Move into the `.wasp/out` directory:

    ```shell
    cd .wasp/out
    ```

2. Link the `.wasp/out` directory to your newly created Railway project:

    ```shell
    railway link
    ```

    Select `app` when prompted to select a service.

3. Go into the Railway dashboard and set up the required env variables:

   Click on the `app` service and go to the **Variables** tab:

   1. Click **Variable reference** and select `DATABASE_URL` (it will populate it with the correct value)

   1. Add `PORT` with the value `8080`, the port you generated the domain for.

   1. Add `WASP_SERVER_URL` with the `app` domain (e.g. `https://app-production-XXXX.up.railway.app`). `https://` prefix is required! There's no separate client URL to configure: `WASP_WEB_CLIENT_URL` defaults to `WASP_SERVER_URL`, and one server serves both your app's pages and its API.

   1. Add `JWT_SECRET` with a random string at least 32 characters long<br /><SecretGeneratorBlock />

     <AddExternalAuthEnvVarsReminder />

4. Push and deploy the project:

    ```shell
    railway up --ci
    ```

    <small>

    We use the `--ci` flag to limit the log output to only the build process.
    </small>

    Railway will locate the `Dockerfile` in `.wasp/out` and build one image with your whole app in it: its pages, its static assets and its API.

And now your Wasp app should be deployed!

Back in your [Railway dashboard](https://railway.com/dashboard?utm_medium=integration&utm_source=docs&utm_campaign=wasp), click on your project and you should see your newly deployed services: PostgreSQL and the app.

### Updates & Redeploying

When you make updates and need to redeploy:

1. Run `wasp build` to rebuild your app.
1. Go into the `.wasp/out` directory and deploy it with:

    ```shell
    railway up --ci
    ```
