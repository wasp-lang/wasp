---
title: Multiple Domains CORS
comments: true
last_checked_with_versions:
  Wasp: "0.21"
---

# Multiple Domains CORS

This guide shows you how to configure CORS (Cross-Origin Resource Sharing) to support multiple domains in your Wasp application using custom global middleware.

## Prerequisites

Make sure you have a Wasp project set up. If you haven't, follow the [Getting Started](../../introduction/quick-start.md) guide first.

## When You Need This

By default, Wasp configures CORS to allow requests only from your client URL (defined by `WASP_WEB_CLIENT_URL`). You might need to support multiple domains when:

- You have multiple domains for the same client application
- You're building a public API
- You're migrating from one domain to another

## Setting up Multiple Domain CORS

### 1. Configure global middleware in main.wasp

Add the server middleware configuration:

```wasp title="main.wasp"
app CorsTest {
  wasp: {
    version: "^0.21.0"
  },
  title: "cors-test",
  server: {
    // highlight-next-line
    middlewareConfigFn: import { getGlobalMiddleware } from "@src/middleware",
  }
}

route RootRoute { path: "/", to: MainPage }
page MainPage {
  component: import { MainPage } from "@src/MainPage"
}

query getSomeData {
  fn: import { getSomeData } from "@src/data",
  entities: []
}
```

### 2. Create the middleware configuration

Create a middleware file that configures CORS with multiple origins:

```ts title="src/middleware.ts"
import cors from "cors";
import { config, type MiddlewareConfigFn } from "wasp/server";

export const getGlobalMiddleware: MiddlewareConfigFn = (middlewareConfig) => {
  // Add extra domains to the existing allowed CORS origins.
  const origin = [
    ...config.allowedCORSOrigins,
    "https://app.example.com",
    "https://admin.example.com",
  ];

  middlewareConfig.set("cors", cors({ origin }));

  return middlewareConfig;
};
```

### 3. Example query handler

Here's an example of a query that will now be accessible from multiple domains:

```ts title="src/data.ts"
import { GetSomeData } from "wasp/server/operations";

export const getSomeData: GetSomeData = async (_args, _context) => {
  return {
    someData: "Hello from the server!",
  };
};
```

## Configuration Options

### Using Environment Variables

You can make the allowed domains configurable via environment variables:

```ts title="src/middleware.ts"
import cors from "cors";
import { config, type MiddlewareConfigFn } from "wasp/server";

export const getGlobalMiddleware: MiddlewareConfigFn = (middlewareConfig) => {
  // Parse additional domains from environment variable.
  const additionalDomains = process.env.CORS_ALLOWED_DOMAINS?.split(",") ?? [];

  const origin = [...config.allowedCORSOrigins, ...additionalDomains];

  middlewareConfig.set("cors", cors({ origin }));

  return middlewareConfig;
};
```

Then in your `.env.server`:

```bash title=".env.server"
CORS_ALLOWED_DOMAINS=https://app.example.com,https://admin.example.com
```

### Dynamic Origin Validation

For more complex scenarios, you can use a function to validate origins:

```ts title="src/middleware.ts"
import cors from "cors";
import { MiddlewareConfigFn } from "wasp/server";

export const getGlobalMiddleware: MiddlewareConfigFn = (config) => {
  config.set(
    "cors",
    cors({
      origin: (origin, callback) => {
        const allowedPatterns = [
          /^https:\/\/.*\.example\.com$/, // Any subdomain of example.com
          /^http:\/\/localhost:\d+$/, // Any localhost port
        ];

        if (
          !origin ||
          allowedPatterns.some((pattern) => pattern.test(origin))
        ) {
          callback(null, true);
        } else {
          callback(new Error("Not allowed by CORS"));
        }
      },
    }),
  );

  return config;
};
```

### Full CORS Configuration

For complete control over CORS, you can set all options:

```ts title="src/middleware.ts"
import cors from "cors";
import { MiddlewareConfigFn } from "wasp/server";

export const getGlobalMiddleware: MiddlewareConfigFn = (config) => {
  config.set(
    "cors",
    cors({
      origin: ["https://app.example.com", "https://admin.example.com"],
      methods: ["GET", "POST", "PUT", "DELETE", "OPTIONS"],
      allowedHeaders: ["Content-Type", "Authorization"],
      credentials: true,
      maxAge: 86400, // 24 hours
    }),
  );

  return config;
};
```

## Security Considerations

- **Never use `origin: "*"` or `origin: [/.*/]` in production** — always restrict origins to specific domains
- Always explicitly list the domains you want to allow
- Consider using environment variables to manage allowed domains across environments
- Regularly audit your allowed domains list
