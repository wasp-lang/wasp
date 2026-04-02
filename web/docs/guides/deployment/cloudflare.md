---
comments: true
---

import BuildingTheWebClient from '../../deployment/deployment-methods/_building-the-web-client.md'

# Cloudflare

## Deploy Wasp to Cloudflare Pages

This guide shows you how to deploy your Wasp app's client to [Cloudflare](https://www.cloudflare.com/) Pages, a free static hosting service. You will need a Cloudflare account to follow these instructions.

Make sure you are logged in with the Cloudflare's CLI called Wrangler. You can log in by running:

```bash
npx wrangler login
```

Before you continue, make sure you have [built the Wasp app](../../deployment/deployment-methods/paas.md#1-generating-deployable-code). We'll build the client web app next.

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

Here's an example configuration file to help you get started. This example workflow will trigger a deployment to Cloudflare Pages whenever changes are pushed to the main branch.

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
          uses: actions/checkout@v5

        - name: Setup Node.js
          id: setup-node
          uses: actions/setup-node@v5
          with:
            node-version: "{minimumNodeJsVersion}"

        - name: Install Wasp
          run: npm i -g @wasp.sh/wasp-cli@{latestWaspVersion} # Change to your Wasp version

        - name: Wasp Build
          run: cd ./app && wasp build

        - name: Build the client
          run: cd ./app && REACT_APP_API_URL=${{ secrets.WASP_SERVER_URL }} npx vite build

        - name: Deploy to Cloudflare Pages
          uses: cloudflare/wrangler-action@v3
          with:
            apiToken: ${{ secrets.CLOUDFLARE_API_TOKEN }}
            accountId: ${{ secrets.CLOUDFLARE_ACCOUNT_ID }}
            command: pages deploy ./app/.wasp/out/web-app/build --project-name=${{ env.CLIENT_CLOUDFLARE_APP_NAME }} --commit-dirty=true --branch=main

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
