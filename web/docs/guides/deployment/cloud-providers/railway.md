---
comments: true
---

import LastCheckedWithVersionsNotice from "@site/src/components/LastCheckedWithVersionsNotice";
import AddExternalAuthEnvVarsReminder from './_addExternalAuthEnvVarsReminder.md'
import { SecretGeneratorBlock } from '../../../project/SecretGeneratorBlock'
import { Server, Client, Database } from '../DeploymentTag'

# Railway

<LastCheckedWithVersionsNotice versions={{ Wasp: "0.24", Railway: new Date("2026-04-06") }} />

## Automatic Deployment <Server /> <Client /> <Database />

We recommend that you use [Wasp Deploy](../../../deployment/deployment-methods/wasp-deploy/railway.md) to deploy your Wasp app to Railway. Wasp CLI automates deploying the app (the server, which also serves the client) and the database with one command.

## Manual Deployment <Server /> <Client /> <Database />

This guide shows you how to deploy your Wasp app and provision a database on Railway.

<Tabs groupId="deployment-mode">
<TabItem value="single" label="Single deployment">

The Docker image Wasp generates contains both the server and the built client, so one Railway service is the whole Wasp app.

</TabItem>
<TabItem value="split" label="Split deployment">

The Docker image Wasp generates contains the server, and the client is a second Railway service serving static files.

</TabItem>
</Tabs>

### Prerequisites

To get started, follow these steps:

1. Make sure your Wasp app is built by running `wasp build` in the project dir (pass any `REACT_APP_*` client env vars to it).
1. Create a [Railway](https://railway.com/?utm_medium=integration&utm_source=docs&utm_campaign=wasp) account.
1. Install the [Railway CLI](https://docs.railway.com/develop/cli?utm_medium=integration&utm_source=docs&utm_campaign=wasp#installing-the-cli).
1. Run `railway login` and a browser tab will open to authenticate you.

### Create New Project

Let's create our Railway project:

1. Go to your [Railway dashboard](https://railway.com/dashboard?utm_medium=integration&utm_source=docs&utm_campaign=wasp), click on **New Project**, and select **Deploy PostgreSQL** from the dropdown menu.
1. Once the project is created, left-click on the **Create** button in the top right corner and select **Empty Service**.
1. Click on the new service, and change the name to `server`.
1. In split mode, create another empty service and name it `client`.
1. Deploy the changes by pressing the **Deploy** button on top.

### Deploy Your App to Railway

#### Setup Domain

We'll need a domain for the `server` service:

1. Go to the `server` instance's **Settings** tab, and click **Generate Domain**.
1. Enter `8080` as the port and click **Generate Domain**.
1. Copy the domain, as we will need it later.
1. In split mode, do the same under the `client`'s **Settings** and copy that domain too.

#### Deploying the App

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

   1. Add `WASP_SERVER_URL` with the `server` domain (e.g. `https://server-production-XXXX.up.railway.app`). `https://` prefix is required! In the default single deployment mode `WASP_WEB_CLIENT_URL` defaults to it, so you don't need to set it; in split mode, add it with the `client` domain.
   1. Add `JWT_SECRET` with a random string at least 32 characters long<br /><SecretGeneratorBlock />

     <AddExternalAuthEnvVarsReminder />

4. Push and deploy the project:

    ```shell
    railway up --ci
    ```

    <small>

    We use the `--ci` flag to limit the log output to only the build process.
    </small>

    Railway will locate the `Dockerfile` in `.wasp/out` and deploy your app. It is now live at the `server` domain. If you use OAuth, register `<server domain>/auth/<provider>/callback` as the redirect URI with your provider.

<Tabs groupId="deployment-mode">
<TabItem value="single" label="Single deployment">

That's the whole app: the pages and Wasp's routes are both served from the `server` domain, and there is nothing else to deploy.

</TabItem>
<TabItem value="split" label="Split deployment">

Now deploy the client to its own service:

1. Create the production build from the project root, using the `server` domain as the `REACT_APP_API_URL`:

    ```shell
    REACT_APP_API_URL=<url_to_wasp_backend> npx vite build
    ```

2. Create a `railway.json` file in `.wasp/out/web-app/build` to ensure Railway uses the correct builder for the static files:

    <!--
      NOTE: When updating this railway.json, make sure to also update it in the deployment package at:
      waspc/data/packages/deploy/src/providers/railway/commands/deploy/client.ts
    -->
    ```json title=".wasp/out/web-app/build/railway.json"
    {
      "$schema": "https://railway.com/railway.schema.json",
      "build": {
        "builder": "RAILPACK"
      }
    }
    ```

3. Create a `Caddyfile` in `.wasp/out/web-app/build` to configure how Railway serves your static files:

    <!--
      NOTE: When updating this Caddyfile, make sure to also update it in the deployment package at:
      waspc/data/packages/deploy/src/providers/railway/commands/deploy/client.ts
    -->
    ```caddyfile title=".wasp/out/web-app/build/Caddyfile"
    {
      admin off
      persist_config off
      auto_https off

      log {
        format json
      }

      servers {
        trusted_proxies static private_ranges
      }
    }

    :{$PORT:80} {
      log {
        format json
      }

      respond /health 200

      # Security headers
      header {
        # Enable cross-site filter (XSS) and tell browsers to block detected attacks
        X-XSS-Protection "1; mode=block"
        # Prevent some browsers from MIME-sniffing a response away from the declared Content-Type
        X-Content-Type-Options "nosniff"
        # Keep referrer data off of HTTP connections
        Referrer-Policy "strict-origin-when-cross-origin"
        # Enable strict Content Security Policy
        Content-Security-Policy "default-src 'self'; img-src 'self' data: https: *; style-src 'self' 'unsafe-inline' https: *; script-src 'self' 'unsafe-inline' https: *; font-src 'self' data: https: *; connect-src 'self' https: *; media-src 'self' https: *; object-src 'none'; frame-src 'self' https: *;"
        # Remove Server header
        -Server
      }

      root * .

      # Handle static files
      file_server {
        hide .git
        hide .env*
      }

      # Compression with more formats
      encode {
        gzip
        zstd
      }

      # Try files with HTML extension and handle SPA routing
      # This is where we diverge from the Railpacks's original Caddyfile
      try_files {path} {path}/index.html /200.html

      handle_errors {
        rewrite * /{err.status_code}.html
        file_server
      }
    }
    ```

    This overrides [Railway's default Caddyfile](https://github.com/railwayapp/railpack/blob/main/core/providers/staticfile/Caddyfile.template) so that prerendered pages are served correctly and non-prerendered routes fall back to the SPA shell (`200.html`).

4. Link the client build directory to the `client` service:

    ```shell
    cd .wasp/out/web-app/build
    railway link
    ```

5. Deploy the client build to Railway:

    ```shell
    railway up --ci
    ```

    Select `client` when prompted to select a service.

</TabItem>
</Tabs>

And now your Wasp should be deployed!

Back in your [Railway dashboard](https://railway.com/dashboard?utm_medium=integration&utm_source=docs&utm_campaign=wasp), click on your project and you should see your newly deployed services: PostgreSQL and Server (plus Client, in split mode).

### Updates & Redeploying

When you make updates and need to redeploy:

1. Run `wasp build` to rebuild your app (with the same `REACT_APP_*` client env vars).
1. Go into the `.wasp/out` directory and deploy with:
    ```shell
    railway up --ci
    ```

In split mode, also rebuild the client from the project root and deploy it:

```shell
REACT_APP_API_URL=<url_to_wasp_backend> npx vite build
cd .wasp/out/web-app/build
railway up --ci
```
