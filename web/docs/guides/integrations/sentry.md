---
last_update:
  date: 2025-06-27
title: Sentry
comments: true
---

# Sentry

This guide shows you how to integrate [Sentry](https://sentry.io/) into your Wasp application for error tracking on both the server and client.

## Prerequisites

- A Wasp project set up
- A [Sentry account](https://sentry.io/signup/)

## Setting up Sentry

### 1. Create Sentry Projects

You'll need to create two projects in Sentry:

1. **Server project**: Select `Node.js` as the platform and `Express` as the framework
2. **Client project**: Select `React` as the platform

After creating each project, you'll receive a unique DSN (Data Source Name) that you'll use to configure Sentry.

### 2. Install Sentry packages

Install the Sentry SDKs:

```bash
npm install @sentry/node @sentry/react
```

### 3. Configure Server-Side Sentry

First, add the server setup function to your `main.wasp`:

```wasp title="main.wasp"
app SentryTest {
  wasp: {
    version: "^0.15.0"
  },
  title: "sentry-test",
  server: {
    setupFn: import { setupFn } from "@src/serverSetup"
  }
}
```

Then create the server setup file:

```ts title="src/serverSetup.ts"
import * as Sentry from "@sentry/node";
import { ServerSetupFn } from "wasp/server";

Sentry.init({
  dsn: "https://your-server-dsn@sentry.io/your-project-id",
  // Optionally add more configuration
  environment: process.env.NODE_ENV,
  tracesSampleRate: 1.0,
});

export const setupFn: ServerSetupFn = async ({ app }) => {
  // Set up Sentry error handler for Express
  Sentry.setupExpressErrorHandler(app);
};
```

:::note
Find your DSN in Sentry under **Settings > Client Keys (DSN)**.
:::

### 4. Configure Client-Side Sentry

Add the client setup function to your `main.wasp`:

```wasp title="main.wasp"
app SentryTest {
  wasp: {
    version: "^0.15.0"
  },
  title: "sentry-test",
  server: {
    setupFn: import { setupFn } from "@src/serverSetup"
  },
  client: {
    setupFn: import { setupFn } from "@src/clientSetup"
  },
}
```

Create the client setup file:

```ts title="src/clientSetup.ts"
import * as Sentry from "@sentry/react";

Sentry.init({
  dsn: "https://your-client-dsn@sentry.io/your-project-id",
  integrations: [],
  // Optionally add more configuration
  environment: import.meta.env.MODE,
  tracesSampleRate: 1.0,
});

export const setupFn = async () => {
  // You can add additional client-side setup here if needed
};
```

:::note
Even if your `setupFn` is empty, you must define and export it - this is how Wasp works.
:::

## Using Environment Variables

For better security, use environment variables for your DSNs:

```ts title="src/serverSetup.ts"
Sentry.init({
  dsn: process.env.SENTRY_SERVER_DSN,
  environment: process.env.NODE_ENV,
});
```

```ts title="src/clientSetup.ts"
Sentry.init({
  dsn: import.meta.env.REACT_APP_SENTRY_CLIENT_DSN,
  environment: import.meta.env.MODE,
});
```

Add to your `.env.server`:

```bash title=".env.server"
SENTRY_SERVER_DSN=https://your-server-dsn@sentry.io/your-project-id
```

Add to your `.env.client`:

```bash title=".env.client"
REACT_APP_SENTRY_CLIENT_DSN=https://your-client-dsn@sentry.io/your-project-id
```

## Testing the Integration

### Test Server Errors

Create an API endpoint that throws an error:

```ts title="src/apis.ts"
import { TestError } from "wasp/server/api";

export const testError: TestError = async (req, res) => {
  throw new Error("Test server error for Sentry");
};
```

### Test Client Errors

Add a button that triggers an error:

```tsx title="src/MainPage.tsx"
export const MainPage = () => {
  const handleError = () => {
    throw new Error("Test client error for Sentry");
  };

  return (
    <div>
      <button onClick={handleError}>Test Sentry Error</button>
    </div>
  );
};
```

## Advanced Configuration

### Adding User Context

Track which user encountered an error:

```ts title="src/serverSetup.ts"
import * as Sentry from "@sentry/node";

// In your API handlers or operations
export const someOperation = async (args, context) => {
  if (context.user) {
    Sentry.setUser({
      id: context.user.id,
      email: context.user.email,
    });
  }
  // ... rest of your code
};
```

### Performance Monitoring

Enable performance monitoring:

```ts
Sentry.init({
  dsn: "your-dsn",
  tracesSampleRate: 0.1, // Capture 10% of transactions
  profilesSampleRate: 0.1, // Capture 10% of profiles (if using profiling)
});
```

### Error Boundaries (React)

Use Sentry's error boundary for React:

```tsx title="src/App.tsx"
import * as Sentry from "@sentry/react";

export const App = ({ children }) => {
  return (
    <Sentry.ErrorBoundary fallback={<p>An error occurred</p>}>
      {children}
    </Sentry.ErrorBoundary>
  );
};
```

For more configuration options, see the [Sentry documentation](https://docs.sentry.io/).
