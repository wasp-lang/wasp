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
    import { expect, test } from "@playwright/test";
    import { generateRandomUser, logUserIn } from "./utils";

    const user = generateRandomUser();

    test.describe("basic user flow test", () => {      
      test("log in and add task", async ({ page }) => {
        await logUserIn({ page, user });
        await expect(page).toHaveURL("/");
        await expect(page.locator("body")).toContainText("No tasks yet.");

        // Add a task
        await page.fill('input[name="description"]', "First task");
        await page.click('input:has-text("Create task")');
        await expect(page.locator("body")).toContainText("First task");
      });
    });
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

## Building the app as a Docker images

The easiest way to continuously deploy your app is to build it as a Docker image. This way you can easily deploy the same image to different environments (dev, staging, production).

**To build the app as a Docker image**, you'll need to:
1. Install Docker in the CI environment.
2. Build the app with `wasp build`.
3. Build the server app with its `Dockerfile` and push it to a Docker registry.
4. Build the client app with a `Dockerfile` and push it to a Docker registry.
5. Notify your deployment provider to deploy the new image.

TODO: We'll take a look at our Coolify deployment example in the [deployment](../deployment) section. Talk about the Github Action. Talk about GHCR and Docker Hub.