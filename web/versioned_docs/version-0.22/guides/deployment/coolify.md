---
comments: true
last_checked_with_versions:
  Wasp: 0.21.0
  Coolify: 2026-01-30
---

import { SecretGeneratorBlock } from "../../project/SecretGeneratorBlock";

# Coolify

## Deploy Wasp with Coolify

This guide shows you how to deploy a Wasp application to [Coolify](https://coolify.io/), a self-hosted deployment platform that makes managing your infrastructure easy.

### Prerequisites

- A server with [Coolify installed](https://coolify.io/self-hosted)
- A domain name
- A GitHub repository with your Wasp application

### Overview

Deploying to Coolify involves:

1. Creating Coolify apps (client, server, and database)
2. Building Docker images using GitHub Actions
3. Triggering Coolify to pull and deploy the images

### Step 1: Set Up Your Domain

Point your DNS A records to your server IP:

- `@` (root) → server IP (for `myapp.com` - client)
- `api` → server IP (for `api.myapp.com` - server)

### Step 2: Create Coolify Resources

#### Create the Database

1. Create a new resource and select **PostgreSQL**
2. Use the default PostgreSQL variant
3. Name it `myapp-db`
4. Click **Start** to set up the database
5. Copy the **Postgres URL (internal)** - you'll need this later

#### Create the Server App

1. Create a new resource and select **Docker Image**
2. Set the image name to `ghcr.io/<your-github-username>/myapp-server`
3. Name it `myapp-server`
4. Configure:
   - **Domains**: `https://api.<your-domain>`
   - **Docker Image Tag**: `main`
   - **Port Exposes**: `3001`
5. Click **Save**

#### Create the Client App

1. Create a new resource and select **Docker Image**
2. Set the image name to `ghcr.io/<your-github-username>/myapp-client`
3. Name it `myapp-client`
4. Configure:
   - **Domains**: `https://<your-domain>`
   - **Docker Image Tag**: `main`
   - **Port Exposes**: `8043`
5. Click **Save**

### Step 3: Configure Server Environment Variables

In the server app, go to **Environment Variables** and add:

| Variable              | Value                                               |
| --------------------- | --------------------------------------------------- |
| `DATABASE_URL`        | The Postgres URL (internal) from step 2             |
| `JWT_SECRET`          | Random string at least 32 characters long: <SecretGeneratorBlock />    |
| `PORT`                | `3001`                                              |
| `WASP_WEB_CLIENT_URL` | `https://<your-domain>`                             |
| `WASP_SERVER_URL`     | `https://api.<your-domain>`                         |

Add any other environment variables your app needs (from `.env.server`).

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

      - name: Trigger Deploy Webhooks
        env:
          CLIENT_COOLIFY_WEBHOOK: ${{ secrets.CLIENT_COOLIFY_WEBHOOK }}
          SERVER_COOLIFY_WEBHOOK: ${{ secrets.SERVER_COOLIFY_WEBHOOK }}
          COOLIFY_TOKEN: ${{ secrets.COOLIFY_TOKEN }}
        run: |
          curl "${{ env.CLIENT_COOLIFY_WEBHOOK }}" --header 'Authorization: Bearer ${{ env.COOLIFY_TOKEN }}'
          curl "${{ env.SERVER_COOLIFY_WEBHOOK }}" --header 'Authorization: Bearer ${{ env.COOLIFY_TOKEN }}'
```

### Step 5: Configure GitHub Secrets

In your GitHub repository, go to **Settings > Secrets and variables > Actions** and add:

#### `SERVER_COOLIFY_WEBHOOK`

1. Go to your server app in Coolify
2. Click **Webhooks**
3. Copy the **Deploy Webhook** URL

#### `CLIENT_COOLIFY_WEBHOOK`

1. Go to your client app in Coolify
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
2. Create Docker images for server and client
3. Push images to GitHub Container Registry
4. Trigger Coolify to deploy the new images
