---
comments: true
---

import LastCheckedWithVersionsNotice from "@site/src/components/LastCheckedWithVersionsNotice";
import { SecretGeneratorBlock } from "../../../project/SecretGeneratorBlock";

# Coolify

<LastCheckedWithVersionsNotice versions={{ Wasp: "0.26", Coolify: new Date("2026-01-30") }} />

## Deploy Wasp with Coolify

This guide shows you how to deploy a Wasp application to [Coolify](https://coolify.io/), a self-hosted deployment platform that makes managing your infrastructure easy.

### Prerequisites

- A server with [Coolify installed](https://coolify.io/self-hosted)
- A domain name
- A GitHub repository with your Wasp application

### Overview

Deploying to Coolify involves:

1. Creating Coolify resources (the app and the database)
2. Building the Docker image using GitHub Actions
3. Triggering Coolify to pull and deploy the image

`wasp build` gives you a single Docker image that contains your whole app: its pages, its static assets, its API and its websockets. That is why there is one app in Coolify, not one for the client and one for the server.

### Step 1: Set Up Your Domain

Point your DNS A record to your server IP:

- `@` (root) → server IP (for `myapp.com`)

### Step 2: Create Coolify Resources

#### Create the Database

1. Create a new resource and select **PostgreSQL**
2. Use the default PostgreSQL variant
3. Name it `myapp-db`
4. Click **Start** to set up the database
5. Copy the **Postgres URL (internal)** - you'll need this later

#### Create the App

1. Create a new resource and select **Docker Image**
2. Set the image name to `ghcr.io/<your-github-username>/myapp`
3. Name it `myapp`
4. Configure:
   - **Domains**: `https://<your-domain>`
   - **Docker Image Tag**: `main`
   - **Port Exposes**: `3001`
5. Click **Save**

### Step 3: Configure Environment Variables

In the app, go to **Environment Variables** and add:

| Variable          | Value                                                               |
| ----------------- | ------------------------------------------------------------------- |
| `DATABASE_URL`    | The Postgres URL (internal) from step 2                             |
| `JWT_SECRET`      | Random string at least 32 characters long: <SecretGeneratorBlock /> |
| `PORT`            | `3001`                                                              |
| `WASP_SERVER_URL` | `https://<your-domain>`                                             |

Add any other environment variables your app needs (from `.env.server`).

:::tip
`WASP_WEB_CLIENT_URL` defaults to `WASP_SERVER_URL`, and your app's pages and its API are on the same origin now, so setting `WASP_SERVER_URL` to your domain is all it takes.
:::

### Step 4: Create GitHub Action

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

      - name: Trigger Deploy Webhook
        env:
          COOLIFY_WEBHOOK: ${{ secrets.COOLIFY_WEBHOOK }}
          COOLIFY_TOKEN: ${{ secrets.COOLIFY_TOKEN }}
        run: |
          curl "${{ env.COOLIFY_WEBHOOK }}" --header 'Authorization: Bearer ${{ env.COOLIFY_TOKEN }}'
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

Anyone can read them in the browser, so never put secrets there. Server environment variables stay where they were, in Coolify (step 3).
:::

### Step 5: Configure GitHub Secrets

In your GitHub repository, go to **Settings > Secrets and variables > Actions** and add:

#### `COOLIFY_WEBHOOK`

1. Go to your app in Coolify
2. Click **Webhooks**
3. Copy the **Deploy Webhook** URL

#### `COOLIFY_TOKEN`

1. In Coolify, go to **Settings** and under **Advanced** enable API Access
2. Go to **Keys & Tokens** > **API tokens**
3. Create a new API token with **Deploy** permissions
4. Copy the token

### Step 6: Deploy

Push to the `main` branch and the GitHub Action will:

1. Build your Wasp application
2. Create a Docker image for it
3. Push the image to GitHub Container Registry
4. Trigger Coolify to deploy the new image

Your app should now be accessible at `https://myapp.com`!
