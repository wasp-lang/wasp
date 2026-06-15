---
title: Testing the build locally
---

import { SecretGeneratorBlock } from '../project/SecretGeneratorBlock'

`wasp build start` lets you test your production build locally before deployment, ensuring everything works correctly before going live.

This command takes the output of `wasp build` and starts a local server to run it. That means that you can test using the same optimized code that would be deployed to production. You also configure it with the same environment variables you'd use in production, which helps you catch configuration issues before deploying.

While it's not identical to a real production environment, it's the closest you can get to testing your deployed app without actually deploying it.

:::warning This is not a deployment command
`wasp build start` is only intended for testing your `wasp build` output locally, and is not designed for serving your app in production. For that, check out our [deployment guide](./intro.md).
:::

## Usage

```bash
# Start a local database, copy the connection URL
wasp start db

# Start the local production build server
# (this is an example, you'll probably need to add more environment variables)
wasp build start --server-env DATABASE_URL=<your-database-url> --server-env JWT_SECRET=<your-jwt-secret>
```

:::tip
For `JWT_SECRET`, you can generate a random secret here: <SecretGeneratorBlock />.

You might need to pass other environment variables as well, depending on your app's configuration. Check our [Environment variables reference](../project/env-vars.md) for more details.
:::

This command will:

- Start a local server serving your production build (the output of `wasp build`).
- Use only the environment variables you set explicitly.
- Use the same bundled assets that would be deployed.
- Run in production mode with optimizations enabled.

## Why?

The main reason for using `wasp build start` is to catch dependencies on your local development environment that might not work in production.

For example, your app might rely on environment variables that are set in your local `.env` files. `wasp start` by default will read these files and use them. While this makes it easy to develop your app locally, it also makes it easy to lose track of which environment variables your app actually needs in production.

`wasp build start` forces you to explicitly specify the environment variables your app needs to run in production. This helps you double-check which ones you also need to set in your deployment environment for the app to work correctly.

Your code might also depend on some development-only features in your libraries, such as React development or strict mode. `wasp build start` runs your app as they would for your users, which means that these features are disabled. This helps you catch issues that might only appear in production.

You should treat this command as the last check before deploying your app, confirming that you know all required environment variables and that integrations behave as expected with production settings on. It is also the best way to reproduce issues that only appear in production, which can be very useful for debugging.

## Differences from `wasp start`

| Aspect                                   | `wasp start`        | `wasp build start`                                |
| ---------------------------------------- | ------------------- | ------------------------------------------------- |
| Runs your app for general production use | **No**              | **No** (check our [deployment guide](./intro.md)) |
| Intended for                             | Local development   | Local production testing                          |
| Server environment                       | Node.js             | Node.js in a Docker container                     |
| Client environment                       | Static server       | Static server                                     |
| Assets                                   | Served individually | Bundled and minified                              |
| React dev mode                           | Enabled             | Disabled                                          |
| Hot reload                               | Enabled             | Disabled                                          |
| Source maps                              | Enabled             | Disabled                                          |
| Debugging support                        | Full                | Limited                                           |
| Performance                              | Slower              | Normal                                            |

## Passing environment variables

You must manually specify any environment variables that your app needs to run in production. This is crucial because the production build may require different configurations from the development build. This helps you take note of which ones you also need to set in your deployment environment for the app to work correctly.

Environment variables include database URLs, API keys, and any other configuration settings necessary for your app to function correctly. You can usually check out your [`.env` files](../project/env-vars.md#dotenv-files) to see what environment variables your app expects. You can read more about environment variables in Wasp in the [environment variables guide](../project/env-vars.md).

The only exception is the environment variables that configure your app's client and server URLs (`WASP_WEB_CLIENT_URL`, `WASP_SERVER_URL`, and `REACT_APP_API_URL`). Because `wasp build start` knows that it's running the app on your local workstation, it can fill them out for you automatically.

### Which values should I use when testing?

- Do not use real production secrets or endpoints.
- Prefer staging/sandbox credentials and services that mirror production (e.g., Stripe test keys, a staging DB, or an isolated local DB with realistic data).
- Keep config parity with production: same feature flags, callbacks/redirect URLs, and optional vars set/unset as in prod.

Example:

```bash
wasp build start --server-env-file .env.staging --client-env-file .env.client.staging
```

### Server environment variables

Use `--server-env` to specify environment variables for the server:

```bash
wasp build start --server-env DATABASE_URL=postgresql://localhost:5432/myapp
```

You can specify multiple server environment variables:

```bash
wasp build start --server-env DATABASE_URL=postgresql://localhost:5432/myapp --server-env JWT_SECRET=my-secret-key
```

You can also point to an `.env` file to load environment variables:

```bash
wasp build start --server-env-file .env.production
```

:::warning
Do not commit your `.env` files with sensitive information to your version control system. Use `.gitignore` to exclude them.
:::

### Client environment variables

Use `--client-env` to specify environment variables for the client:

```bash
wasp build start --client-env REACT_APP_GOOGLE_ANALYTICS_ID=GA-123456
```

Multiple client environment variables:

```bash
wasp build start --client-env REACT_APP_GOOGLE_ANALYTICS_ID=GA-123456 --client-env REACT_APP_PLAUSIBLE_ID=PLAUSIBLE-123456
```

You can also point to an `.env` file for client variables:

```bash
wasp build start --client-env-file .env.client.production
```

:::warning
Do not commit your `.env` files with sensitive information to your version control system. Use `.gitignore` to exclude them.
:::
