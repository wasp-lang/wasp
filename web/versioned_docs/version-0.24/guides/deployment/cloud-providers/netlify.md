---
comments: true
last_checked_with_versions:
  Wasp: "0.24"
  Netlify: 2026-05-28
---

import BuildingTheWebClient from '../../../deployment/deployment-methods/_building-the-web-client.md'
import { Client } from '../DeploymentTag'

# Netlify

## Deploy Wasp to Netlify <Client />
This guide shows you how to deploy your Wasp app's client to Netlify. Netlify is a static hosting solution that is free for many use cases. You will need a Netlify account to follow these instructions.

Make sure you are logged in with Netlify CLI. You can check if you are logged in with `npx netlify-cli status`, and if you are not, you can log in with `npx netlify-cli login`.

First, make sure you have [built the Wasp app](../../../deployment/deployment-methods/cloud-providers.md#1-generating-deployable-code). We'll build the client web app next.

<BuildingTheWebClient />

Before deploying, you need to create a `netlify.toml` file in your project root to tell Netlify where to find the built client and to configure URL redirects for SPA routing.

Create the `netlify.toml` file with the following content:

```toml title="netlify.toml"
[build]
  publish = "./.wasp/out/web-app/build"

# By default, Netlify only redirects when a path doesn't match an existing file.
# See: https://docs.netlify.com/manage/routing/redirects/rewrites-proxies/#shadowing
[[redirects]]
  from = "/*"
  to = "/200.html"
  status = 200
```

The `build.publish` path should point from the directory containing `netlify.toml` to the built client output. Adjust the path if your Wasp project is in a subdirectory (e.g., `publish = "./my-app/.wasp/out/web-app/build"`).

We can now deploy the client with:

```shell
npx netlify-cli deploy --filter wasp --no-build
```

<small>
  Carefully follow the instructions: decide if you want to create a new app or use an existing one, pick the team under which your app will be deployed etc. Netlify CLI detects Wasp's generated server and SDK packages as workspaces, so `--filter wasp` makes that selection explicit. The `build.publish` setting still determines which files Netlify uploads. The `--no-build` flag is used because you already built the client with the right environment variables.
</small>

The final step is to run:

```shell
npx netlify-cli deploy --prod --filter wasp --no-build
```

That is it! Your client should be live at `https://<app-name>.netlify.app`.

:::note
Make sure you set the `https://<app-name>.netlify.app` URL as the `WASP_WEB_CLIENT_URL` environment variable in your server hosting environment.
:::

### Deploying through GitHub Actions

To enable automatic deployment of the client whenever you push to the `main` branch, you can set up a GitHub Actions workflow. To do this, create a file in your repository at `.github/workflows/deploy.yaml`. Feel free to rename `deploy.yaml` as long as the file type is not changed.

Here's an example configuration file to help you get started. This example workflow will trigger a deployment to Netlify whenever changes are pushed to the main branch.

<details>
  <summary>Example GitHub Action</summary>

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
          uses: actions/checkout@v5

        - name: Setup Node.js
          id: setup-node
          uses: actions/setup-node@v5
          with:
            node-version: "{minimumNodeJsVersion}"

        - name: Install Wasp
          run: npm i -g @wasp.sh/wasp-cli@{latestWaspVersion} # Change to your Wasp version

        - name: Wasp Install
          run: wasp install

        - name: Wasp Build
          run: wasp build

        - name: Build the client
          run: REACT_APP_API_URL=${{ secrets.WASP_SERVER_URL }} npx vite build

        - name: Deploy to Netlify
          run: |
            npx netlify-cli deploy --prod --auth=$NETLIFY_AUTH_TOKEN --site=$NETLIFY_SITE_ID --filter wasp --no-build

      env:
        NETLIFY_AUTH_TOKEN: ${{ secrets.NETLIFY_AUTH_TOKEN }}
        NETLIFY_SITE_ID: ${{ secrets.NETLIFY_SITE_ID }}
  ```
</details>

<details>
  <summary>How do I get the Environment Variables?</summary>

  - **`NETLIFY_AUTH_TOKEN`**: For the auth token, you'll generate a new Personal Access Token on [Netlify](https://docs.netlify.com/cli/get-started/#obtain-a-token-in-the-netlify-ui).

  - **`NETLIFY_SITE_ID`**: This is the ID of your Netlify project.

  - **`WASP_SERVER_URL`**: This is your server's URL and is generally only available after **deploying the backend**. This variable can be skipped when the backend is not functional or not deployed, but be aware that backend-dependent functionalities may be broken.

  After getting the environment variables, you need to set these in GitHub Repository Secrets.
</details>
