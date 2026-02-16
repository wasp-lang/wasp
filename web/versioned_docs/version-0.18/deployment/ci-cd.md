---
title: CI/CD Scenarios
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

We'll look at two ways you can use the CI/CD pipeline to deploy your Wasp app:

1. Package the server and client with Docker.
2. Deploy the client as static files.

### Package the server and client with Docker

The most common way to package your app for deployment is using Docker images. This way you can easily deploy the same image to different environments (staging, production, etc.).

**To build the app as a Docker image**, you'll need to:

1. Install Docker in the CD environment.
2. Build the app with `wasp build`.
3. Build the Docker image and push it to a Docker registry:
   - for our server app
   - for our client app
4. For some providers: notify them to deploy the new app version.

:::info What is a Docker Registry?

Docker Registry is a place where you can store your Docker images and then your deployment provider can pull them from there. The most common Docker Registry is the [Docker Hub](https://hub.docker.com/), but you can also use other registries like the [Github Container Registry (GHCR)](https://docs.github.com/en/packages/guides/about-github-container-registry).

:::

#### Example deployment

We'll take a look at our Coolify deployment example in the [deployment](./deployment-methods/self-hosted.md#coolify) section. We are using Github Actions to build the Docker images and their Github Container Registry (GHCR) to store them.

Let's go through the [deploy.yml](https://gist.github.com/infomiho/ad6fade7396498ae32a931ca563a4524#file-deploy-yml) file in the Coolify guide:

1. First, we **authenticate with the Github Container Registry (GHCR)**.

   We are using the `docker/login-action` action to authenticate with the GHCR.

2. Then, we **prepare the Docker image metadata** for later use.

   We are using the `docker/metadata-action` action to prepare some extra info that we'll use later in the deployment process.

3. Next, we **build the Wasp app** with `wasp build`.

   This gives our server and the client app in the `.wasp/build` folder.

4. Then, we **package the server app** into a Docker image and **push it to the GHCR**.

   We use the `Dockerfile` in the `.wasp/build` directory to build and push the server Docker image using the `docker/build-push-action` action.

5. Next, we create a `Dockerfile` for our client and then **package the client app** into a Docker image and **push it to the GHCR**.

   We create a `Dockerfile` that uses a simple Go static server to serve the client app. We again use the `docker/build-push-action` action to build and push the client Docker image.

6. Finally, we notify Coolify using their Webhook API to **deploy our new app version**.

   And now you can open the [deploy.yml](https://gist.github.com/infomiho/ad6fade7396498ae32a931ca563a4524#file-deploy-yml) file in the Coolify guide and see the full deployment process.

### Static build of the client

Wasp's client app is a single page application (SPA) which you build into static HTML, CSS, and JS files that you can upload to any hosting provider that supports serving static files. This means that for the client app, you don't need to use Docker images if don't want to. It's usually cheaper to host static files than to host Docker images.

**To deploy the client app as static files**, you'll need to:

1. Build the app with `wasp build` in the CD environment.
2. Build the client app with `npm run build`.
3. Upload the static files to your hosting provider.

<!-- TOOD: update links below -->

Check out our instructions for deploying the client app to [Netlify](./deployment-methods/paas.md#netlify) or [Cloudflare](./deployment-methods/paas.md#cloudflare) where you can check out the example deployment using Github Actions.
