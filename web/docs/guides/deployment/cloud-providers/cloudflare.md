---
comments: true
last_checked_with_versions:
  Wasp: "0.23"
  "Cloudflare Workers": 2026-04-06
---

import BuildingTheWebClient from '../../../deployment/deployment-methods/_building-the-web-client.md'
import { Client } from '../DeploymentTag'

# Cloudflare

## Deploy Wasp to Cloudflare Workers <Client />

This guide shows you how to deploy your Wasp app's client to [Cloudflare](https://www.cloudflare.com/) Workers, a free hosting service. You will need a Cloudflare account to follow these instructions.

Make sure you are logged in with the Cloudflare's CLI called Wrangler. You can log in by running:

```bash
npx wrangler login
```

Before you continue, make sure you have [built the Wasp app](../../deployment/deployment-methods/paas.md#1-generating-deployable-code). We'll build the client web app next.

<BuildingTheWebClient />

To deploy the client to Cloudflare Workers, create these two files in the root of your project:

1. A `wrangler.toml` that configures the Worker with static assets:

```toml title="wrangler.toml"
name = "my-wasp-app-client"
main = "./worker.js"
compatibility_date = "2026-03-30"

[assets]
directory = "./.wasp/out/web-app/build"
binding = "ASSETS"
```

2. And a `worker.js` that serves static files and falls back to the SPA shell for unknown routes:

```js title="worker.js"
export default {
  async fetch(request, env) {
    // If the static asset is not found, return the SPA fallback.
    const spaFallbackUrl = new URL("/200", request.url);
    const spaFallbackRequest = new Request(spaFallbackUrl, request);
    return await env.ASSETS.fetch(spaFallbackRequest);
  },
};
```
Keeping these files in the project root ensures they are tracked in your repository.

Finally, deploy from your project root:

```shell
npx wrangler deploy
```

That is it! Your client should be live at `https://my-wasp-app-client.<subdomain>.workers.dev`.

:::note
Make sure you set your Workers URL as the `WASP_WEB_CLIENT_URL` environment variable in your server hosting environment.
:::

### Deploying through Github Actions

To enable automatic deployment of the client whenever you push to the `main` branch, you can set up a GitHub Actions workflow. To do this, create a file in your repository at `.github/workflows/deploy.yaml`. Feel free to rename `deploy.yaml` as long as the file type is not changed.

Here's an example configuration file to help you get started. This example workflow will trigger a deployment to Cloudflare Workers whenever changes are pushed to the main branch.

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

        - name: Deploy to Cloudflare Workers
          uses: cloudflare/wrangler-action@v3
          with:
            apiToken: ${{ secrets.CLOUDFLARE_API_TOKEN }}
            accountId: ${{ secrets.CLOUDFLARE_ACCOUNT_ID }}
            workingDirectory: ./app
            command: deploy
  ```
</details>

<details>
  <summary>How do I get the Environment Variables?</summary>

  - **`CLOUDFLARE_API_TOKEN` and `CLOUDFLARE_ACCOUNT_ID`**: You can get these from your [Cloudflare dashboard](https://dash.cloudflare.com/profile/api-tokens). Make sure to give the token `Cloudflare Workers: Edit` permissions.

  - **`WASP_SERVER_URL`**: This is your server's URL and is generally only available after **deploying the backend**. This variable can be skipped when the backend is not functional or not deployed, but be aware that backend-dependent functionalities may be broken.

  After getting the environment variables, you need to set these in GitHub Repository Secrets.
</details>
