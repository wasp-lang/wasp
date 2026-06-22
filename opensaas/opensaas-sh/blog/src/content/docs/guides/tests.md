---
title: Tests
banner:
  content: |
    Have an Open SaaS app in production? <a href="https://e44cy1h4s0q.typeform.com/to/EPJCwsMi">We'll send you some swag! 👕</a>
---

This guide will show you how to use the included end-to-end (e2e) tests for your Open SaaS application.

## The Tests Directory

In the root of your project, you'll find an `e2e-tests` directory which contains the [Playwright](https://playwright.dev) tests for your Open SaaS application.:

```
.
├── e2e-tests/
│   ├── tests/                  # Directory containing the test files
│   ├── README.md               # Instructions on how to run the tests
│   ├── ci-start-app-and-db.js  # Script to start the app and db for CI
│   ├── playwright.config.ts    # Playwright configuration
│   ├── package.json
│   ├── ...
```

To run the tests locally, or in a CI pipeline, follow the instructions in the `README.md` file in the `e2e-tests` directory.

## Using Tests in CI with GitHub Actions
Although the Open SaaS template does not come with an example workflow, you can find one at `.github/workflows/e2e-tests.yml` of the [remote repo](https://github.com/wasp-lang/open-saas).

You can copy and paste the `.github/` directory containing the `e2e-tests.yml` workflow into the root of your own repository to run the tests as part of your CI pipeline.

:::caution[WASP_VERSION]
Please make sure to update the `WASP_VERSION` environment variable in the `e2e-tests.yml` file to match the version of Wasp you are using in your project.
:::

In order for these tests to run correctly on GitHub, you need to provide the environment variables mentioned in the `e2e-tests.yml` file within your GitHub repository's "Actions" secrets so that they can be accessed by the tests.
