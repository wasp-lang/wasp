---
title: Production Build Preview
---

The `wasp build start` command allows you to preview your production build locally before deploying. This helps you test how your app behaves in production conditions and verify that it works correctly with your production configuration.

This command takes the output of `wasp build` and starts a local server to preview it. This runs your app with the same optimized build and very similar environment to how it will be deployed to your users.

:::warning This is not a deployment command
While this command uses your production build, it runs your app without many of the security and reliability guarantess that our [supported deployment methods](../deployment/intro.md) have. When you're ready to deploy your app to the world, check out our [deployment guide](../deployment/intro.md) for instructions and best practices.
:::

## Usage

```bash
wasp build start --server-env DATABASE_URL=<your-database-url> --server-env JWT_SECRET=<your-jwt-secret>
```

This command will:
- Start a local server serving your production build.
- Use the same bundled assets that would be deployed
- Run in production mode with optimizations enabled



## Environment Variables

You have to manually specify any environment variables that your app needs to run in production. This is crucial because the production build may require different configurations than the development build. This helps you make sure of which environment variables you need to set in your deployment environment for your app to work correctly.

This includes database URLs, API keys, and any other configuration settings that are necessary for your app to function correctly. You can usually check out your [`.env` files](./env-vars.md#dotenv-files) to see what environment variables your app expects. You can read more about environment variables in Wasp in the [environment variables guide](./env-vars.md).

The only exception are the environment variables that configure your app's client and server URLs (`WASP_WEB_CLIENT_URL`, `WASP_SERVER_URL`, or `REACT_APP_API_URL`). Because `wasp build start` knows that it's running the app on your local workstation, it can fill them out for you automatically.


### Server Environment Variables

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

### Client Environment Variables

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

## Differences from `wasp start`

| Aspect | `wasp start` | `wasp build start` |
|--------|--------------|--------------------|
| Runs your app for general production use | **No** | **No** (check our [deployment guide](../deployment/intro.md)) |
| Intended for | Local development | Local production preview |
| Server environment | Node.js | Node.js in a Docker container |
| Client environment | Static server | Static server |
| Assets | Served individually | Bundled and minified |
| React dev mode | Enabled | Disabled |
| Hot reload | Enabled | Disabled |
| Source maps | Full | Optimized/minimal |
| Debugging support | Full | Limited |
| Performance | Slower | Normal |
