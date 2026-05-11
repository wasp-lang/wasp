---
comments: true
last_checked_with_versions:
  Wasp: 0.21.0
  Caprover: 2026-01-30
---

import { SecretGeneratorBlock } from "../../project/SecretGeneratorBlock";

# Caprover

## Deploy Wasp with Caprover

This guide shows you how to deploy a Wasp application to [Caprover](https://caprover.com/), a self-hosted PaaS (Platform as a Service) for managing your deployments.

### Prerequisites

- A server with [Caprover installed](https://caprover.com/docs/get-started.html#prerequisites)
- A domain name
- A GitHub repository with your Wasp application

### Overview

Deploying to Caprover involves:

1. Creating Caprover apps (client, server, and database)
2. Building Docker images using GitHub Actions
3. Triggering Caprover to deploy the images

### Step 1: Set Up Your Domain

Point your DNS A records to your server IP:

- `@` (root) → server IP (for `myapp.com` - client)
- `api` → server IP (for `api.myapp.com` - server)

:::tip
If you followed Caprover's install instructions with `*.apps` subdomain setup, you can use `https://myapp-client.apps.mydomain.com` and `https://myapp-server.apps.mydomain.com` for quick testing.
:::

### Step 2: Create Caprover Apps

#### Create the Database

1. Go to **One-Click Apps** and select **PostgreSQL**
2. Name it `myapp-db`
3. Set version to `18` (or whichever version is latest)
4. Deploy it
5. Note the connection string: `postgresql://postgres:<password>@srv-captain--myapp-db:5432/postgres`

#### Create the Server App

1. Create a new app named `myapp-server`
2. Go to **HTTP Settings**:
   - Connect domain `https://api.<your-domain>`
   - Click **Enable HTTPS**
   - Set **Container HTTP Port** to `3001`
   - Enable **Force HTTPS** and **Websocket Support**
3. Click **Save & Restart**

#### Create the Client App

1. Create a new app named `myapp-client`
2. Go to **HTTP Settings**:
   - Connect domain `https://<your-domain>`
   - Click **Enable HTTPS**
   - Set **Container HTTP Port** to `8043`
   - Enable **Force HTTPS** and **Websocket Support**
3. Click **Save & Restart**

### Step 3: Configure Server Environment Variables

In the server app, go to **App Configs > Environment Variables** and add:

| Variable              | Value                                                                  |
| --------------------- | ---------------------------------------------------------------------- |
| `DATABASE_URL`        | `postgresql://postgres:<password>@srv-captain--myapp-db:5432/postgres` |
| `JWT_SECRET`          | Random string at least 32 characters long: <SecretGeneratorBlock />    |
| `PORT`                | `3001`                                                                 |
| `WASP_WEB_CLIENT_URL` | `https://<your-domain>`                                                |
| `WASP_SERVER_URL`     | `https://api.<your-domain>`                                            |

Add any other environment variables your app needs (from `.env.server`).

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
  SERVER_APP_NAME: "myapp-server"
  SERVER_APP_URL: "https://api.myapp.com"
  CLIENT_APP_NAME: "myapp-client"
  DOCKER_REGISTRY: "ghcr.io"
  DOCKER_REGISTRY_USERNAME: ${{ github.repository_owner }}
  DOCKER_REGISTRY_PASSWORD: ${{ secrets.GITHUB_TOKEN }}

jobs:
  build-and-push-images:
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

      - name: (server) Extract metadata for Docker
        id: meta-server
        uses: docker/metadata-action@v5
        with:
          images: ${{ env.DOCKER_REGISTRY }}/${{ env.DOCKER_REGISTRY_USERNAME }}/${{ env.SERVER_APP_NAME }}

      - name: (client) Extract metadata for Docker
        id: meta-client
        uses: docker/metadata-action@v5
        with:
          images: ${{ env.DOCKER_REGISTRY }}/${{ env.DOCKER_REGISTRY_USERNAME }}/${{ env.CLIENT_APP_NAME }}

      - name: Setup Node.js
        uses: actions/setup-node@v6
        with:
          node-version: "{minimumNodeJsVersion}"

      - name: Install Wasp
        shell: bash
        run: npm i -g @wasp.sh/wasp-cli@${{ env.WASP_VERSION }}

      # Uncomment if using Wasp TS Config
      # - name: Initialize Wasp TS Config
      #   run: wasp ts-setup

      - name: Build Wasp app
        run: wasp build

      - name: (client) Build
        run: REACT_APP_API_URL=${{ env.SERVER_APP_URL }} npx vite build

      - name: (client) Prepare Dockerfile
        run: |
          cd ./.wasp/out/web-app
          echo "FROM pierrezemb/gostatic" > Dockerfile
          echo "CMD [\"-fallback\", \"index.html\", \"-enable-logging\"]" >> Dockerfile
          echo "COPY ./build /srv/http" >> Dockerfile

      - name: (server) Build and push Docker image
        uses: docker/build-push-action@v6
        with:
          # Remove 'app/' if your app is at the repo root
          context: ./app/.wasp/out
          file: ./app/.wasp/out/Dockerfile
          push: true
          tags: ${{ steps.meta-server.outputs.tags }}
          labels: ${{ steps.meta-server.outputs.labels }}

      - name: (client) Build and push Docker image
        uses: docker/build-push-action@v6
        with:
          # Remove 'app/' if your app is at the repo root
          context: ./app/.wasp/out/web-app
          file: ./app/.wasp/out/web-app/Dockerfile
          push: true
          tags: ${{ steps.meta-client.outputs.tags }}
          labels: ${{ steps.meta-client.outputs.labels }}

      - name: (server) Deploy to Caprover
        uses: caprover/deploy-from-github@v1.1.2
        with:
          server: ${{ secrets.CAPROVER_SERVER }}
          app: ${{ env.SERVER_APP_NAME }}
          token: ${{ secrets.SERVER_APP_TOKEN }}
          image: ${{ steps.meta-server.outputs.tags }}

      - name: (client) Deploy to Caprover
        uses: caprover/deploy-from-github@v1.1.2
        with:
          server: ${{ secrets.CAPROVER_SERVER }}
          app: ${{ env.CLIENT_APP_NAME }}
          token: ${{ secrets.CLIENT_APP_TOKEN }}
          image: ${{ steps.meta-client.outputs.tags }}
```

### Step 6: Configure GitHub Secrets

In your GitHub repository, go to **Settings > Secrets and variables > Actions** and add:

#### `CAPROVER_SERVER`

Your Caprover dashboard URL, e.g., `https://captain.apps.mydomain.com`

#### `SERVER_APP_TOKEN`

1. Go to your server app in Caprover
2. Under **Deployment**, find **Method 1: Official CLI**
3. Click **Enable App Token**
4. Copy the token

#### `CLIENT_APP_TOKEN`

1. Go to your client app in Caprover
2. Under **Deployment**, find **Method 1: Official CLI**
3. Click **Enable App Token**
4. Copy the token

### Step 7: Deploy

Push to the `main` branch and the GitHub Action will:

1. Build your Wasp application
2. Create Docker images for server and client
3. Push images to GitHub Container Registry
4. Deploy both apps to Caprover
