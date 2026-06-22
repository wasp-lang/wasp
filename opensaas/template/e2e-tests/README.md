# Open SaaS e2e Tests with Playwright

These are e2e tests that are written using [Playwright](https://playwright.dev/) for the Open SaaS project.

They not only serve as tests for development of the Open SaaS project, but also as reference examples for how you can implement tests for the app you build with Open SaaS as a template, if you choose to do so.

## Running the tests

### Locally

First, make sure you've [integrated Stripe into your app](https://docs.opensaas.sh/guides/stripe-integration/). This includes [installing the Stripe CLI and logging into it](https://docs.opensaas.sh/guides/stripe-testing/) with your Stripe account.

Next, Install the test dependencies:

```shell
cd e2e-tests && npm install
```

Start your Wasp DB and leave it running:

```shell
cd ../app && wasp db start
```

### Skipping Email Verification in e2e Tests

Open another terminal and start the Wasp app with the environment variable set to skip email verification in development mode:

> [!IMPORTANT]  
> When using the email auth method, a verification link is typically sent when a user registers. If you're using the default Dummy provider, this link is logged in the console.
>
> **However, during e2e tests, this manual step will cause the tests to hang and fail** because the link is never clicked. To prevent this, set the following environment variable when starting your app:

```bash
cd app && SKIP_EMAIL_VERIFICATION_IN_DEV=true wasp start
```

#### What this step will do:

- **Automated Testing:** Skipping email verification ensures e2e tests run uninterrupted.
- **Consistent Behavior:** It guarantees login flows won’t break during automated test runs.
- **CI/CD Pipelines:** This variable should also be set in CI pipelines to avoid test failures.
  ```yaml
  env:
    SKIP_EMAIL_VERIFICATION_IN_DEV: "true"
  ```

In another terminal, run the local e2e tests:

```shell
cd e2e-tests && npm run local:e2e:start
```

This will start the tests in Playwright's UI mode, which will allow you to see and run the tests in an interactive browser environment. You should also see the Stripe events being triggered in the terminal where the tests were started.

To exit the local e2e tests, go back to the terminal were you started your tests and press `ctrl + c`.

## CI/CD

Although the Open SaaS template does not come with an example workflow, you can find one at `.github/workflows/e2e-tests.yml` of the [remote repo](https://github.com/wasp-lang/open-saas).

You can copy and paste the `.github/` directory containing the `e2e-tests.yml` workflow into the root of your own repository to run the tests as part of your CI pipeline.

> [!IMPORTANT]  
> Please make sure to update the `WASP_VERSION` environment variable in the `e2e-tests.yml` file to match the version of Wasp you are using in your project.

In order for these tests to run correctly on GitHub, you need to provide the environment variables mentioned in the `e2e-tests.yml` file within your GitHub repository's "Actions" secrets so that they can be accessed by the tests.

Upon pushing to the repository's main branch, or creating a PR against the main branch, the tests will run in the CI/CD pipeline.
