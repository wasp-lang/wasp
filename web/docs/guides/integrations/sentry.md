---
title: "Sentry: error tracking in a Wasp app"
sidebar_label: Sentry
comments: true
---

import LastCheckedWithVersionsNotice from "@site/src/components/LastCheckedWithVersionsNotice";

# Sentry

<LastCheckedWithVersionsNotice versions={{ Wasp: "0.24", "@sentry/node": "8", "@sentry/react": "8" }} />

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

### 3. Configure your Wasp file

Add the server and client instrumentation functions, and a server setup function, to your `main.wasp.ts`:

```ts title="main.wasp.ts"
import { app } from "@wasp/spec"
import { registerClientSentry } from "./src/clientInstrumentation" with { type: "ref" }
import { registerServerSentry } from "./src/serverInstrumentation" with { type: "ref" }
import { setupServer } from "./src/serverSetup" with { type: "ref" }

export default app({
  name: "MyApp",
  wasp: { version: "^0.27.0" },
  title: "my-app",
  head: ["<link rel='icon' href='/favicon.ico' />"],
  server: {
    // highlight-next-line
    instrumentation: registerServerSentry,
    // highlight-next-line
    setupFn: setupServer,
  },
  client: {
    // highlight-next-line
    instrumentation: registerClientSentry,
  },
  // ...
})
```

Wasp calls the [instrumentation functions](../../advanced/server-customization/server-config.md#instrumentation) before it loads any other code, so Sentry can observe your whole app.

### 4. Configure Server-Side Sentry

Create the server instrumentation file:

```ts title="src/serverInstrumentation.ts" auto-js
import * as Sentry from "@sentry/node";

export async function registerServerSentry(): Promise<void> {
  Sentry.init({
    dsn: process.env.SENTRY_SERVER_DSN,
    environment: process.env.NODE_ENV,
    tracesSampleRate: 1.0,
  });
}
```

Then add Sentry's error handler to the Express app in the server setup function:

```ts title="src/serverSetup.ts" auto-js
import * as Sentry from "@sentry/node";
import { ServerSetupFn } from "wasp/server";

export const setupServer: ServerSetupFn = async ({ app }) => {
  Sentry.setupExpressErrorHandler(app);
};
```

:::note
Find your DSN in Sentry under **Settings > Client Keys (DSN)**.
:::

### 5. Configure Client-Side Sentry

Create the client instrumentation file:

```ts title="src/clientInstrumentation.ts" auto-js
import * as Sentry from "@sentry/react";

export async function registerClientSentry(): Promise<void> {
  Sentry.init({
    dsn: import.meta.env.REACT_APP_SENTRY_CLIENT_DSN,
    environment: import.meta.env.MODE,
    tracesSampleRate: 1.0,
  });
}
```

### 6. Set up environment variables

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

```ts title="src/apis.ts" auto-js
import { TestError } from "wasp/server/api";

export const testError: TestError = async (req, res) => {
  throw new Error("Test server error for Sentry");
};
```

### Test Client Errors

Add a button that triggers an error:

```tsx title="src/MainPage.tsx" auto-js
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

```ts title="src/serverSetup.ts" auto-js
import * as Sentry from "@sentry/node";

// In your API handlers or operations
export const someOperation = async (args, context) => {
  if (context.user) {
    Sentry.setUser({
      id: context.user.id,
      email: context.user.email,
    });
  }
  // ...
};
```

### Performance Monitoring

Enable performance monitoring:

```ts auto-js
Sentry.init({
  dsn: "your-dsn",
  tracesSampleRate: 0.1, // Capture 10% of transactions
  profilesSampleRate: 0.1, // Capture 10% of profiles (if using profiling)
});
```

### Error Boundaries (React)

Use Sentry's error boundary for React:

```tsx title="src/App.tsx" auto-js
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
