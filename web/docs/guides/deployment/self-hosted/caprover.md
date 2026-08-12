---
comments: true
---

import LastCheckedWithVersionsNotice from "@site/src/components/LastCheckedWithVersionsNotice";
import { SecretGeneratorBlock } from "../../../project/SecretGeneratorBlock";

# Caprover

<LastCheckedWithVersionsNotice versions={{ Wasp: "0.26", Caprover: new Date("2026-01-30") }} />

## Deploy Wasp with Caprover

This guide shows you how to deploy a Wasp application to [Caprover](https://caprover.com/), a self-hosted PaaS (Platform as a Service) for managing your deployments.

### Prerequisites

- A server with [Caprover installed](https://caprover.com/docs/get-started.html#prerequisites)
- A domain name
- A GitHub repository with your Wasp application

### Overview

Deploying to Caprover involves:

1. Creating Caprover apps (the app and the database)
2. Building the Docker image using GitHub Actions
3. Triggering Caprover to deploy the image

`wasp build` gives you a single Docker image that contains your whole app: its pages, its static assets, its API and its websockets. That is why there is one app in Caprover, not one for the client and one for the server.

### Step 1: Set Up Your Domain

Point your DNS A record to your server IP:

- `@` (root) → server IP (for `myapp.com`)

:::tip
If you followed Caprover's install instructions with `*.apps` subdomain setup, you can use `https://myapp.apps.mydomain.com` for quick testing.
:::

### Step 2: Create Caprover Apps

#### Create the Database

1. Go to **One-Click Apps** and select **PostgreSQL**
2. Name it `myapp-db`
3. Set version to `18` (or whichever version is latest)
4. Deploy it
5. Note the connection string: `postgresql://postgres:<password>@srv-captain--myapp-db:5432/postgres`

#### Create the App

1. Create a new app named `myapp`
2. Go to **HTTP Settings**:
   - Connect domain `https://<your-domain>`
   - Click **Enable HTTPS**
   - Set **Container HTTP Port** to `3001`
   - Enable **Force HTTPS** and **Websocket Support**
3. Click **Save & Restart**

### Step 3: Configure Environment Variables

In the app, go to **App Configs > Environment Variables** and add:

| Variable          | Value                                                                  |
| ----------------- | ---------------------------------------------------------------------- |
| `DATABASE_URL`    | `postgresql://postgres:<password>@srv-captain--myapp-db:5432/postgres` |
| `JWT_SECRET`      | Random string at least 32 characters long: <SecretGeneratorBlock />    |
| `PORT`            | `3001`                                                                 |
| `WASP_SERVER_URL` | `https://<your-domain>`                                                |

Add any other environment variables your app needs (from `.env.server`).

:::tip
`WASP_WEB_CLIENT_URL` defaults to `WASP_SERVER_URL`, and your app's pages and its API are on the same origin now, so setting `WASP_SERVER_URL` to your domain is all it takes.
:::

### Step 4: Enable GitHub Container Registry Access

1. In Caprover, go to **Cluster**
2. Add a new **Remote Registry**:
   - **Username**: Your GitHub username
   - **Password**: Your GitHub personal access token
   - **Domain**: `ghcr.io`
   - **Image Prefix**: Your GitHub username

### Step 5: Create GitHub Action

Create `.github/workflows/deploy.yml` in your repository:

```yaml title=".github/workflows/deploy.yml"
name: "Deploy"

on:
  push:
    branches:
      - "main"

concurrency:
  group: deployment
  cancel-in-progress: true

env:
  WASP_VERSION: "{pinnedLatestWaspVersion}"
  APP_NAME: "myapp"
  DOCKER_REGISTRY: "ghcr.io"
  DOCKER_REGISTRY_USERNAME: ${{ github.repository_owner }}
  DOCKER_REGISTRY_PASSWORD: ${{ secrets.GITHUB_TOKEN }}

jobs:
  build-and-push-image:
    permissions:
      contents: read
      packages: write
    runs-on: ubuntu-latest
    # Remove this block if your app is NOT in an 'app' folder
    defaults:
      run:
        working-directory: ./app
    steps:
      - name: Checkout repository
        uses: actions/checkout@v4

      - name: Log in to Container registry
        uses: docker/login-action@v3
        with:
          registry: ghcr.io
          username: ${{ env.DOCKER_REGISTRY_USERNAME }}
          password: ${{ env.DOCKER_REGISTRY_PASSWORD }}

      - name: Extract metadata for Docker
        id: meta
        uses: docker/metadata-action@v5
        with:
          images: ${{ env.DOCKER_REGISTRY }}/${{ env.DOCKER_REGISTRY_USERNAME }}/${{ env.APP_NAME }}

      - name: Setup Node.js
        uses: actions/setup-node@v6
        with:
          node-version: "{minimumNodeJsVersion}"

      - name: Install Wasp
        shell: bash
        run: npm i -g @wasp.sh/wasp-cli@${{ env.WASP_VERSION }}

      - name: Install Wasp app dependencies
        run: wasp install

      - name: Build Wasp app
        run: wasp build

      - name: Build and push Docker image
        uses: docker/build-push-action@v6
        with:
          # Remove 'app/' if your app is at the repo root
          context: ./app/.wasp/out
          file: ./app/.wasp/out/Dockerfile
          push: true
          tags: ${{ steps.meta.outputs.tags }}
          labels: ${{ steps.meta.outputs.labels }}

      - name: Deploy to Caprover
        uses: caprover/deploy-from-github@v1.1.2
        with:
          server: ${{ secrets.CAPROVER_SERVER }}
          app: ${{ env.APP_NAME }}
          token: ${{ secrets.APP_TOKEN }}
          image: ${{ steps.meta.outputs.tags }}
```

:::note Client environment variables
Environment variables prefixed with `REACT_APP_` end up inside your app's pages and assets, so they have to be there when the image is built, not when it runs. If your app has any, pass them to the image build with the `WASP_CLIENT_ENV` build argument, as shell assignments:

```yaml
      - name: Build and push Docker image
        uses: docker/build-push-action@v6
        with:
          # ...
          build-args: |
            WASP_CLIENT_ENV=REACT_APP_EXAMPLE='value'; REACT_APP_OTHER='another value'
```

Anyone can read them in the browser, so never put secrets there. Server environment variables stay where they were, in Caprover (step 3).
:::

### Step 6: Configure GitHub Secrets

In your GitHub repository, go to **Settings > Secrets and variables > Actions** and add:

#### `CAPROVER_SERVER`

Your Caprover dashboard URL, e.g., `https://captain.apps.mydomain.com`

#### `APP_TOKEN`

1. Go to your app in Caprover
2. Under **Deployment**, find **Method 1: Official CLI**
3. Click **Enable App Token**
4. Copy the token

### Step 7: Deploy

Push to the `main` branch and the GitHub Action will:

1. Build your Wasp application
2. Create a Docker image for it
3. Push the image to GitHub Container Registry
4. Deploy the app to Caprover

Your app should now be accessible at `https://myapp.com`!
