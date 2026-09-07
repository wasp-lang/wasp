---
title: From 0.25 to 0.26
---

# Migration from 0.25 to 0.26

import InstallInstructions from './\_install-instructions.md'

<InstallInstructions version="0.26" />

## What's new in 0.26?

### Running multiple Wasp apps side by side

`wasp start` now decides which ports the client and the server run on. If the default ports (`3000` and `3001`) are taken, it moves the app to free ones instead of failing, so you can run several Wasp apps side by side.

This is great for using agents in parallel worktrees, as each one won't conflict with the other.

You can also pick the ports yourself:

```bash
wasp start --client-port 4000 --server-port 4001
wasp build start --client-port 4000 --server-port 4001
```

In development, setting the ports manually in the env vars or the Vite config now fails, and you should use these new CLI flags. In production, the `PORT` variable is now required.

### Server base path

The whole server can now live under a path of your choice with the new [`server.basePath`](./project/server-config.md#base-path) option: Wasp's own routes (`/auth/*`, `/operations/*`, `/crud/*`), your `api`s and anything you add in `setupFn`. New projects set it to `/api`; if you don't set it, it stays `/` and nothing moves.

## How to migrate?

### 1. Bump the Wasp version

Update the version field in your Wasp config to `^0.26.0`.

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```ts title="main.wasp.ts"
    export default app({
      // highlight-next-line
      wasp: { version: "^0.25.0" },
      // ...
    });
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```ts title="main.wasp.ts"
    export default app({
      // highlight-next-line
      wasp: { version: "^0.26.0" },
      // ...
    });
    ```
  </TabItem>
</Tabs>

And run the following command to update the Wasp libraries in your project:

```bash
wasp install
```

### 2. Update your TypeScript config

Due to `wasp/sdk` package changes, we require some changes to your TypeScript configuration.

In `tsconfig.src.json`, update the `include` field:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```json title="tsconfig.src.json"
    {
      "compilerOptions": {
        // ...
      },
      // highlight-next-line
      "include": ["src"]
    }
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```json title="tsconfig.src.json"
    {
      "compilerOptions": {
        // ...
      },
      // highlight-next-line
      "include": ["src", ".wasp/out/types/app"]
    }
    ```
  </TabItem>
</Tabs>

### 3. Stop setting the dev ports and URLs yourself

Wasp now picks the ports your app runs on in development and derives its URLs from them, so it fails if you also set them.

If your `vite.config.ts` sets `server.port` or `server.strictPort`, remove it:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```ts title="vite.config.ts"
    export default defineConfig({
      // highlight-start
      server: {
        port: 4000,
      },
      // highlight-end
      plugins: [wasp()],
    });
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```ts title="vite.config.ts"
    export default defineConfig({
      plugins: [wasp()],
    });
    ```
  </TabItem>
</Tabs>

You should also remove `PORT`, `WASP_SERVER_URL` and `WASP_WEB_CLIENT_URL` from your `.env.server` if you were setting those manually:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```env title=".env.server"
    # highlight-start
    PORT=4001
    WASP_SERVER_URL=http://localhost:4001
    WASP_WEB_CLIENT_URL=http://localhost:4000
    # highlight-end
    JWT_SECRET=my-secret
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```env title=".env.server"
    JWT_SECRET=my-secret
    ```
  </TabItem>
</Tabs>

And `REACT_APP_API_URL` from your `.env.client` if present:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```env title=".env.client"
    # highlight-next-line
    REACT_APP_API_URL=http://localhost:4001
    REACT_APP_NAME=My App
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```env title=".env.client"
    REACT_APP_NAME=My App
    ```
  </TabItem>
</Tabs>

To keep running on those ports, pass them to `wasp start` or `wasp build start` instead:

```bash
wasp start --client-port 4000 --server-port 4001
```

Wasp fills in the URLs for you from the ports it picked, so you no longer have to keep them in sync by hand.

:::info

Your deployed app still uses these environment variables, so don't remove them from your deploy configuration. Wasp only takes them over in development, where it is the one starting your app.
:::

### 4. Set `PORT` in deployment

**If you use `wasp deploy fly` or `wasp deploy railway` to deploy your app, you can skip this step.**

`PORT` used to fall back to `3001` when you didn't set it. It no longer has a default, so the server refuses to start without it.

Most deployment platforms set `PORT` for you, but it's worth it to check it in their documentation or your deployment configuration.

```env title="Server env vars"
PORT=3001
```

### 5. Update your custom Dockerfile

If you are using a [custom Dockerfile](./deployment/deployment-methods/overview#customizing-the-dockerfile), due to `wasp/sdk` package changes,
you'll have to add a one new additional line to it:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```dockerfile title="Dockerfile"
    # ...
    COPY sdk .wasp/out/sdk
    COPY libs .wasp/out/libs
    # ...
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```dockerfile title="Dockerfile"
    # ...
    COPY sdk .wasp/out/sdk
    // highlight-next-line
    COPY types .wasp/out/types
    COPY libs .wasp/out/libs
    # ...
    ```
  </TabItem>
</Tabs>


### 6. Update Fly database deployment flags

If you use database sizing options with `wasp deploy fly launch` or `wasp deploy fly create-db`, rename them as follows:

| Before                  | After                              |
| ----------------------- | ---------------------------------- |
| `--vm-size`             | `--db-vm-size`                     |
| `--initial-cluster-size` | `--db-initial-cluster-size`        |
| `--volume-size`         | `--db-volume-size`                 |

### 7. Set the server base path (optional)

The server now mounts under `server.basePath`: Wasp's routes, your `api` and `apiNamespace` routes, and anything you add in `setupFn`. New projects use `/api`, and we recommend it: it keeps the server's routes under one prefix. Add it to your Wasp file:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```ts title="main.wasp.ts"
    export default app({
      server: {
        setupFn: mySetupFunction,
      },
      // ...
    });
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```ts title="main.wasp.ts"
    export default app({
      server: {
        // highlight-next-line
        basePath: "/api",
        setupFn: mySetupFunction,
      },
      // ...
    });
    ```
  </TabItem>
</Tabs>

If you'd rather not move anything, leave it out: the base path stays `/`, Wasp's routes stay at `/auth/*`, `/operations/*` and `/crud/*`, your `api` paths stay where they are, and your OAuth redirect URIs don't change. You can skip the rest of this step.

If you set it, a few things move with it:

- Your `api` and `apiNamespace` paths are relative to the base path, so an API declared at `/foo/bar` is now served at `/api/foo/bar`. Calls through the `api` instance from `wasp/client/api` follow along (`api.get("/foo/bar")` now hits `/api/foo/bar`), but update external callers and any hardcoded URLs.
- Routes you register directly on the Express app in `setupFn` move under the base path too, since the app you get is mounted there: `app.get("/customRoute", ...)` now answers on `/api/customRoute`. The server root (`GET /`) stays where it was.
- Server code that builds URLs to the server's own routes from `config.serverUrl` needs `config.serverBasePath` in between: `${config.serverUrl}${config.serverBasePath}/foo`.
- Socket.IO now listens at `<basePath>/socket.io`. Wasp's `useSocket` follows along, but a custom `socket.io-client` you create yourself must pass `path: "/api/socket.io"`.
- If you use Google, GitHub, Discord, Keycloak, Microsoft or Slack auth, the redirect URI you registered with the provider now includes the base path. Do this for every provider and every environment (development, staging, production). The [social auth pages](./auth/social-auth/overview.md) have the updated values.

  | Before                                       | After                                            |
  | -------------------------------------------- | ------------------------------------------------ |
  | `http://localhost:3001/auth/google/callback` | `http://localhost:3001/api/auth/google/callback` |
  | `https://api.myapp.com/auth/google/callback` | `https://api.myapp.com/api/auth/google/callback` |

- On the client, `config.apiUrl` from `wasp/client` is now the URL of the API, the server's origin plus the base path (for example `http://localhost:3001/api`), rather than the bare server URL. `REACT_APP_API_URL` and `WASP_SERVER_URL` stay the server's origin, Wasp appends the base path itself. String concatenation like `` `${config.apiUrl}/foo` `` keeps pointing at your `api` declared at `/foo`; code that used `config.apiUrl` as the server's origin needs `new URL(config.apiUrl).origin` instead.
- Hand-written `msw` handlers in client tests that hardcode the server URL need the API URL instead. `mockQuery` and `mockApi` from `wasp/client/test` keep working.

  <Tabs sideBySide>
    <TabItem value="before" label="Before">
      ```ts title="src/MainPage.test.tsx"
      http.post("http://localhost:3001/operations/get-tasks", () =>
        HttpResponse.json([]),
      )
      ```
    </TabItem>
    <TabItem value="after" label="After">
      ```ts title="src/MainPage.test.tsx"
      // highlight-next-line
      import { config } from "wasp/client";

      // highlight-next-line
      http.post(`${config.apiUrl}/operations/get-tasks`, () =>
        HttpResponse.json([]),
      )
      ```
    </TabItem>
  </Tabs>

### 8. Enjoy your updated Wasp app

That's it!
