---
title: "Sentry: error tracking in a Wasp app"
sidebar_label: Sentry
comments: true
---

import LastCheckedWithVersionsNotice from "@site/src/components/LastCheckedWithVersionsNotice";

# Sentry

<LastCheckedWithVersionsNotice versions={{ Wasp: "0.27", "@sentry/node": "11", "@sentry/react": "11" }} />

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

Wasp calls the `onError` functions with every unexpected error on the [server](../../advanced/server-customization/server-config.md#handling-unexpected-errors) and the [client](../../advanced/client-customization/client-config.md#handling-unexpected-errors). Add both to your `main.wasp.ts`:

```ts title="main.wasp.ts"
import { app } from "@wasp/spec"
import { onClientError } from "./src/clientSentry" with { type: "ref" }
import { onServerError } from "./src/serverSentry" with { type: "ref" }

export default app({
  name: "MyApp",
  wasp: { version: "^0.27.0" },
  title: "my-app",
  head: ["<link rel='icon' href='/favicon.ico' />"],
  server: {
    // highlight-next-line
    onError: onServerError,
  },
  client: {
    // highlight-next-line
    onError: onClientError,
  },
  // ...
})
```

### 4. Configure Server-Side Sentry

Create the server file:

```ts title="src/serverSentry.ts" auto-js
import * as Sentry from "@sentry/node";
import { type ServerOnErrorFn } from "wasp/server";

Sentry.init({
  dsn: process.env.SENTRY_SERVER_DSN,
  environment: process.env.NODE_ENV,
  tracesSampleRate: 1.0,
});

export const onServerError: ServerOnErrorFn = async (error, context) => {
  Sentry.captureException(error, { tags: { source: context.source } });

  // The server exits after these errors, so we wait for Sentry to send them.
  if (context.source === "startup" || context.source === "dbSeed") {
    await Sentry.flush(2000);
  }
};
```

Wasp calls `onServerError` for errors in requests, Jobs, WebSockets, OAuth logins, server startup, and database seeds. It doesn't call it for an `HttpError` you throw yourself, since that is an answer you chose to send to the client.

:::note
Find your DSN in Sentry under **Settings > Client Keys (DSN)**.
:::

:::caution Don't use `Sentry.setupExpressErrorHandler`
Sentry's Express error handler never runs in a Wasp app, because Wasp answers failed requests itself. Use `onError` instead.
:::

### 5. Configure Client-Side Sentry

Create the client file:

```ts title="src/clientSentry.ts" auto-js
import * as Sentry from "@sentry/react";
import { type ClientOnErrorFn } from "wasp/client";

Sentry.init({
  dsn: import.meta.env.REACT_APP_SENTRY_CLIENT_DSN,
  environment: import.meta.env.MODE,
  tracesSampleRate: 1.0,
});

export const onClientError: ClientOnErrorFn = (error, context) => {
  // Sentry already listens for these errors itself.
  if (context.source === "window") {
    return;
  }
  Sentry.captureException(error, { tags: { source: context.source } });
};
```

Sentry catches errors in event handlers and unhandled promise rejections on its own. `onClientError` adds the errors Sentry can't see, like an error thrown while rendering a page, which Wasp's error page catches.

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

```ts title="main.wasp.ts"
import { api, app } from "@wasp/spec"
import { testError } from "./src/apis" with { type: "ref" }

export default app({
  // ...
  spec: [
    api("GET", "/test-error", testError, { auth: false }),
  ],
})
```

```ts title="src/apis.ts" auto-js
import { type TestError } from "wasp/server/api";

export const testError: TestError = async (req, res) => {
  throw new Error("Test server error for Sentry");
};
```

Open `http://localhost:3001/test-error` and check that the error shows up in your Sentry server project with the `source: request` tag.

### Test Client Errors

Add a button that throws while rendering:

```tsx title="src/MainPage.tsx" auto-js
import { useState } from "react";

export const MainPage = () => {
  const [shouldThrow, setShouldThrow] = useState(false);

  if (shouldThrow) {
    throw new Error("Test client error for Sentry");
  }

  return (
    <div>
      <button onClick={() => setShouldThrow(true)}>Test Sentry Error</button>
    </div>
  );
};
```

Click the button and check that the error shows up in your Sentry client project with the `source: render` tag.

## Advanced Configuration

### Adding User Context

Track which user encountered an error:

```ts title="src/operations.ts" auto-js
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
