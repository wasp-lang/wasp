---
title: CI/CD Overview
---

Setting up a CI/CD pipeline is an optional but highly recommended part of deploying applications.

**Continuous Integration (CI)** involves verifying/testing code changes through an automated process whenever code is pushed to the repository. This helps us catch bugs early and make sure that our app works.

**Continuous Deployment (CD)** refers to the automatic deployment of code changes to the production environment. This is commonly know as "push to deploy" and frees developers from having to manually deploy code changes.

## Running tests in CI

### End to end tests

End to end (e2e) tests simulate real user using your app and you can test different scenarios like login, adding items to cart, etc. Writing end to end tests frees you from
manually testing your app after every change.

**To run e2e tests with Wasp in the CI**, you'll need to:

1. Install Wasp in the CI environment.
2. Run your app (with the database) in the CI environment.
3. Run the e2e tests against the running app.

#### Example app

We'll show you how to run end-to-end tests in CI using the [Github Actions](https://github.com/features/actions) as our CI and the [Playwright](https://playwright.dev/) as our e2e testing framework.

1. Check our example app and its e2e tests in the [e2e-tests](https://github.com/wasp-lang/e2e-test-example/tree/main/e2e-tests) directory.

   You can copy the `e2e-tests` directory to your own project and modify it to fit your app. This will enable you to run the e2e tests locally.

   <details>
     <summary>Example e2e test</summary>

     ```ts
     import { expect, test } from '@playwright/test'
     import { generateRandomUser, logUserIn } from './utils'

     const user = generateRandomUser()

     test.describe('basic user flow test', () => {
       test('log in and add task', async ({ page }) => {
         await logUserIn({ page, user })
         await expect(page).toHaveURL('/')
         await expect(page.locator('body')).toContainText('No tasks yet.')

         // Add a task
         await page.fill('input[name="description"]', 'First task')
         await page.click('input:has-text("Create task")')
         await expect(page.locator('body')).toContainText('First task')
       })
     })
     ```
   </details>

2. To run the tests in the Github Actions CI, you'll need to create a workflow file in your repository.

   You should create a `.github/workflows/e2e-tests.yml` file in your repository. You can copy the contents of the [e2e-tests.yml](https://github.com/wasp-lang/e2e-test-example/blob/main/.github/workflows/e2e-tests.yml) file from our example app.

### Unit tests

Unit tests test pieces of your code logic in isolation. They are much simpler and faster than e2e tests, but they don't simulate the real user interaction with your app.

You can use Wasp's built in [client tests](../project/testing.md) support to test the client side code of your app. You are free to use any testing framework for the server side code.

**You'd run the unit tests in the CI** in a similar way as the e2e tests:

1. Install Wasp in the CI environment.
2. Run the client tests with `wasp test client run`.
3. Run the server tests with your testing framework.

## Continuous deployment

How you deploy from CI depends on your [deployment mode](./intro.md#deployment-modes). In the default single deployment mode you package the whole app into one Docker image. In split mode you do that for the server and upload the client's static files separately.

### Package the app with Docker

The most common way to package your app for deployment is using Docker images. This way you can easily deploy the same image to different environments (staging, production, etc.).

<Tabs groupId="deployment-mode">
<TabItem value="single" label="Single deployment">

The image Wasp generates contains the server and the built client, so one image is the whole app.

**To build the app as a Docker image**, you'll need to:

1. Install Docker in the CD environment.
2. Install dependencies with `wasp install` and build the app with `wasp build`. Pass any [client env vars](./env-vars.md#client-env-vars) to `wasp build`, since it builds the client.
3. Build the Docker image from `.wasp/out` and push it to a Docker registry.
4. For some providers: notify them to deploy the new app version.

</TabItem>
<TabItem value="split" label="Split deployment">

The image Wasp generates contains the server. You deploy the client separately, as described below.

**To build the server as a Docker image**, you'll need to:

1. Install Docker in the CD environment.
2. Install dependencies with `wasp install` and build the server with `wasp build`.
3. Build the Docker image from `.wasp/out` and push it to a Docker registry.
4. For some providers: notify them to deploy the new app version.

</TabItem>
</Tabs>

:::info What is a Docker Registry?

Docker Registry is a place where you can store your Docker images and then your deployment provider can pull them from there. The most common Docker Registry is the [Docker Hub](https://hub.docker.com/), but you can also use other registries like the [Github Container Registry (GHCR)](https://docs.github.com/en/packages/guides/about-github-container-registry).

:::

#### Example deployment

We'll take a look at our Coolify deployment example in the [deployment](../guides/deployment/self-hosted/coolify.md) section. We are using Github Actions to build the Docker images and their Github Container Registry (GHCR) to store them.

Let's go through the [deploy.yml](https://gist.github.com/infomiho/ad6fade7396498ae32a931ca563a4524#file-deploy-yml) file in the Coolify guide:

1. First, we **authenticate with the Github Container Registry (GHCR)**.

   We are using the `docker/login-action` action to authenticate with the GHCR.

2. Then, we **prepare the Docker image metadata** for later use.

   We are using the `docker/metadata-action` action to prepare some extra info that we'll use later in the deployment process.

3. Next, we **install dependencies** with `wasp install` and **build the Wasp app** with `wasp build`.

   This creates our server and the built client in the `.wasp/out` folder.

4. Then, we **package the app** into a Docker image and **push it to the GHCR**.

   We use the `Dockerfile` in the `.wasp/out` directory to build and push the Docker image using the `docker/build-push-action` action.

5. Finally, we notify Coolify using their Webhook API to **deploy our new app version**.

   And now you can open the [deploy.yml](https://gist.github.com/infomiho/ad6fade7396498ae32a931ca563a4524#file-deploy-yml) file in the Coolify guide and see the full deployment process.

### Static build of the client

This step only applies in [split mode](./intro.md#deployment-modes). Wasp's client is a single page application (SPA) which you build into static HTML, CSS, and JS files and upload to any hosting provider that supports serving static files. In the default single deployment mode the client is already in the Docker image and there is nothing to do here.

**To deploy the client as static files**, you'll need to:

1. Install dependencies with `wasp install` and build the app with `wasp build` in the CD environment.
2. Build the client with `REACT_APP_API_URL=<server_origin> npx vite build`, so it talks to your separately deployed server.
3. Upload the static files (from `.wasp/out/web-app/build`) to your hosting provider.
4. Set `WASP_WEB_CLIENT_URL` on the server to the client's URL.

<!-- TOOD: update links below -->

Check out our instructions for deploying the client app to [Netlify](../guides/deployment/cloud-providers/netlify.md) or [Cloudflare](../guides/deployment/cloud-providers/cloudflare.md) where you can check out the example deployment using Github Actions.
