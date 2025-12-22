---
title: CI/CD Deployment
---

import { WaspDeployProvidersGrid } from './WaspDeployProvidersGrid';

You can use CI/CD platforms like Github Actions to re-deploy your application automatically whenever changes are pushed to your repository.

## Re-deploying from CI/CD

### Prerequisites

Make sure to first deploy your application from your local machine using `wasp deploy <provider> launch`. The `launch` command creates services for your application in the deployment provider and deploys them. After your application is deployed, you are able to use `wasp deploy <provider> deploy` in a CI/CD workflow to re-deploy it.

<WaspDeployProvidersGrid />

### Deployment steps

To automate deployment, you need to create a workflow file in your repository that specifies the deployment process when a new commit is pushed to the repository. 

The workflow needs to include the following steps:

1. Checkout the code from the repository
2. Install Node.js and the Wasp CLI
3. Install any provider-specific dependencies
4. Deploy the application using `wasp deploy <provider> deploy`

To be able to deploy your apps to the deployment providers, you need to set **provider-specific API keys** in the
environment variables. How you set environment variables depends on the CI/CD platform you are using. We'll show you how to do this for [Github Actions](#github-actions-workflow) in the next section.

## Github Actions workflow {#github-actions-workflow}

Let's take a look at an example CI/CD workflow for Github Actions for each of the supported providers.

<Tabs>
<TabItem value="fly" label="Fly.io">

You'll need an **organisation token** to deploy to Fly.io. You can generate the token by running [`fly tokens create org`](https://fly.io/docs/security/tokens/#create-org-scoped-tokens) and adding it to your repository secrets as `FLY_API_TOKEN`.

```yaml title=".github/workflows/deploy.yml"
name: Wasp Deploy

on:
  push:
    branches:
      - main

jobs:
  deploy:
    runs-on: ubuntu-latest
    env:
      WASP_VERSION: 0.19.0

    steps:
      - uses: actions/checkout@v6

      - name: Setup Node.js
        uses: actions/setup-node@v6
        with:
          node-version: '22'

      - name: Install Wasp
        # We pin the Wasp CLI version to avoid issues when a new Wasp version is released.
        run: curl -sSL https://get.wasp.sh/installer.sh | sh -s -- -v $WASP_VERSION

      - name: Install Flyctl
        uses: superfly/flyctl-actions/setup-flyctl@master

      - name: Deploy
        run: wasp deploy fly deploy
        env:
          # You must add FLY_API_TOKEN to your Repository Secrets
          FLY_API_TOKEN: ${{ secrets.FLY_API_TOKEN }}
```

</TabItem>
<TabItem value="railway" label="Railway">

You'll need an **account token** to deploy to Railway. You can generate it by going to your account settings under [Tokens](https://railway.com/account/tokens) and generating a token without selecting a workspace. Set the token as a repository secret named `RAILWAY_API_TOKEN`.

Make sure to replace `my-project-name` with the actual name of your project and `MY_PROJECT_ID` with the actual ID of your project. You can find the project ID in the project's settings.

```yaml title=".github/workflows/deploy.yml"
name: Wasp Deploy

on:
  push:
    branches:
      - main

jobs:
  deploy:
    runs-on: ubuntu-latest
    env:
      WASP_VERSION: 0.19.0
      RAILWAY_PROJECT_NAME: my-project-name
      RAILWAY_PROJECT_ID: MY_PROJECT_ID

    steps:
      - uses: actions/checkout@v6

      - name: Setup Node.js
        uses: actions/setup-node@v6
        with:
          node-version: '22'

      - name: Install Wasp
        # We pin the Wasp CLI version to avoid issues when a new Wasp version is released.
        run: curl -sSL https://get.wasp.sh/installer.sh | sh -s -- -v $WASP_VERSION

      - name: Install Railway CLI
        run: npm install -g @railway/cli

      - name: Deploy
        run: wasp deploy railway deploy $RAILWAY_PROJECT_NAME --existing-project-id $RAILWAY_PROJECT_ID
        env:
          # You must add RAILWAY_API_TOKEN to your Repository Secrets
          RAILWAY_API_TOKEN: ${{ secrets.RAILWAY_API_TOKEN }}
```
</TabItem>
</Tabs>
