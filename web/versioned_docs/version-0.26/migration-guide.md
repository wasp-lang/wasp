---
sidebar_label: From 0.25 to 0.26
---

import InstallInstructions from './_install-instructions.md'

# Migration from 0.25 to 0.26

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

## How to migrate?

### 1. Upgrade Node.js to 24.22.1 or later {#upgrade-nodejs}

Wasp 0.26.1 requires Node.js >= 24.22.1 (previously >= 24.14.1), which includes fixes for the [October 2026 Node.js security vulnerabilities](https://nodejs.org/en/blog/vulnerability/october-2026-security-releases).

The most severe one is a remote code execution bug in Node.js's Brotli decoder (CVE-2026-111497). If you deploy with the Dockerfile Wasp generates, rebuilding with Wasp 0.26.1 also moves your server image to `node:24.22.1-alpine3.23`. If you use a custom Dockerfile or a platform that picks its own Node.js version, make sure it runs 24.22.1 or newer.

Make sure to upgrade before continuing:

```shell
node -v
# If below 24.22.1, upgrade:
nvm install 24
```

### 2. Bump the Wasp version

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

### 3. Update your TypeScript config

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

### 4. Stop setting the dev ports and URLs yourself

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

If you were setting those variables to make your app reachable at a URL other than `localhost` (e.g. a LAN address or an HTTPS tunnel), use `--client-url` and `--server-url` instead. Wasp derives `REACT_APP_API_URL` from the server URL. These URLs don't change the ports, so if a URL contains a port, pin it with the matching port option:

```bash
wasp start \
  --client-port 3000 --client-url http://192.168.1.39.nip.io:3000 \
  --server-port 3001 --server-url http://192.168.1.39.nip.io:3001
```

:::info

Your deployed app still uses these environment variables, so don't remove them from your deploy configuration. Wasp only takes them over in development, where it is the one starting your app.
:::

### 5. Set `PORT` in deployment

**If you use `wasp deploy fly` or `wasp deploy railway` to deploy your app, you can skip this step.**

`PORT` used to fall back to `3001` when you didn't set it. It no longer has a default, so the server refuses to start without it.

Most deployment platforms set `PORT` for you, but it's worth it to check it in their documentation or your deployment configuration.

```env title="Server env vars"
PORT=3001
```

### 6. Update your custom Dockerfile

If you are using a [custom Dockerfile](./deployment/methods/overview#customizing-the-dockerfile), due to `wasp/sdk` package changes,
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


### 7. Update Fly database deployment flags

If you use database sizing options with `wasp deploy fly launch` or `wasp deploy fly create-db`, rename them as follows:

| Before                  | After                              |
| ----------------------- | ---------------------------------- |
| `--vm-size`             | `--db-vm-size`                     |
| `--initial-cluster-size` | `--db-initial-cluster-size`        |
| `--volume-size`         | `--db-volume-size`                 |

### 8. Point liveness checks at `/up`

Wasp now serves a liveness check at `GET /up` in both development and production.

Wasp used `/` for the liveness check before, but it behaved differently per environment:
- Development: `GET /` showed Wasp's wrong-port page.
- Production: `GET /` answered `200 OK`.

We decided to keep the wrong-port page at `/` in development, but we register it after user `api`s, so any user `api` will win over it.
In production, `GET /` is no longer set by Wasp.

This breaks the Caddy setup from the [VPS deployment guide](./guides/deployment/self-hosted/vps.md), which probed `/`. Change its health check to `/up`:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```caddyfile title="Caddyfile"
    api.myapp.com {
        reverse_proxy localhost:3001 {
            health_uri /
            lb_try_duration 15s
        }
    }
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```caddyfile title="Caddyfile"
    api.myapp.com {
        reverse_proxy localhost:3001 {
            // highlight-next-line
            health_uri /up
            lb_try_duration 15s
        }
    }
    ```
  </TabItem>
</Tabs>

If anything else in your deployment probed `GET /`, such as a platform health check or an uptime monitor, point it at `/up` too.

### 9. Update deployment CLIs

Update Fly CLI to 0.4.82 or newer and Railway CLI to 5.28.0 or newer, both locally and in CI, if you use them.

### 10. Replace `wasp info` with `wasp show`

`wasp info` is superseded by `wasp show`. Use `wasp show build --json` in automation scripts to read the last build's Wasp version from `lastCompile.waspVersion`.

### 11. Update selectors for email auth fields

If you are running e2e tests on your app, update selectors from `input[type="email"]` to `input[name="email"]`.

Wasp's login, signup, and password reset forms now use `type="text"` for email fields to support internationalized addresses.

### 12. Enjoy your updated Wasp app

That's it!
