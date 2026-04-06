---
comments: true
last_checked_with_versions:
  Wasp: "0.23"
  Netlify: 2026-04-06
---

import BuildingTheWebClient from '../../../deployment/_building-the-web-client.md'
import NetlifyTomlConfig from '../../../deployment/_netlify-toml-config.md'
import { Client } from '../DeploymentTag'

# Netlify

## Deploy Wasp to Netlify <Client />
This guide shows you how to deploy your Wasp app's client to Netlify. Netlify is a static hosting solution that is free for many use cases. You will need a Netlify account to follow these instructions.

Make sure you are logged in with Netlify CLI. You can check if you are logged in with `npx netlify-cli status`, and if you are not, you can log in with `npx netlify-cli login`.

First, make sure you have [built the Wasp app](../../deployment/deployment-methods/paas.md#1-generating-deployable-code). We'll build the client web app next.

<BuildingTheWebClient />

Before deploying, you need to create a `netlify.toml` file in your project root to configure Netlify for building and deploying your Wasp app. This file tells Netlify where to find the web app and configures URL redirects for SPA routing.

Create the `netlify.toml` file with the following content:

<NetlifyTomlConfig />

The `build.base` path should point from your Git repository root to the `web-app` directory. Adjust the path if your Wasp project is in a subdirectory (e.g., `base = "./my-app/.wasp/out/web-app"`).

The `build.command` is set to `exit 0` because the client is already built with the client environment variables.

We can now deploy the client with:

```shell
npx netlify-cli deploy
```

<small>
  Carefully follow the instructions: decide if you want to create a new app or use an existing one, pick the team under which your app will be deployed etc.
</small>

The final step is to run:

```shell
npx netlify-cli deploy --prod
```

That is it! Your client should be live at `https://<app-name>.netlify.app`.

:::note
Make sure you set the `https://<app-name>.netlify.app` URL as the `WASP_WEB_CLIENT_URL` environment variable in your server hosting environment.
:::

### Deploying through Github Actions

To enable automatic deployment of the client whenever you push to the `main` branch, you can set up a GitHub Actions workflow. To do this, create a file in your repository at `.github/workflows/deploy.yaml`. Feel free to rename `deploy.yaml` as long as the file type is not changed.

Here's an example configuration file to help you get started. This example workflow will trigger a deployment to Netlify whenever changes are pushed to the main branch.

<details>
  <summary>Example Github Action</summary>

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

        - name: Wasp Build
          run: wasp build

        - name: Build the client
          run: REACT_APP_API_URL=${{ secrets.WASP_SERVER_URL }} npx vite build

        - name: Deploy to Netlify
          run: |
            npx netlify-cli deploy --prod --auth=$NETLIFY_AUTH_TOKEN --site=$NETLIFY_SITE_NAME

      env:
        NETLIFY_AUTH_TOKEN: ${{ secrets.NETLIFY_AUTH_TOKEN }}
        NETLIFY_SITE_NAME: netlify-site-name
  ```
</details>

<details>
  <summary>How do I get the Environment Variables?</summary>

  - **`NETLIFY_AUTH_TOKEN`**: For the auth token, you'll generate a new Personal Access Token on [Netlify](https://docs.netlify.com/cli/get-started/#obtain-a-token-in-the-netlify-ui).

  - **`NETLIFY_SITE_NAME`**: This is the name of your Netlify project.

  - **`WASP_SERVER_URL`**: This is your server's URL and is generally only available after **deploying the backend**. This variable can be skipped when the backend is not functional or not deployed, but be aware that backend-dependent functionalities may be broken.

  After getting the environment variables, you need to set these in GitHub Repository Secrets.
</details>
