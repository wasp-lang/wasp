---
title: From 0.25 to 0.26
---

# Migration from 0.25 to 0.26

import InstallInstructions from './\_install-instructions.md'

<InstallInstructions version="0.26" />

## What's new in 0.26?

### One app instead of two

Wasp apps now deploy as a single app: the server serves the built client, and the browser talks to one origin. There is no separate client app to build, host or point at the server anymore.

- `wasp build` builds the client too, and the generated Dockerfile packages it with the server.
- `wasp build start` runs one container, and `wasp deploy fly` / `wasp deploy railway` create one app plus the database.
- In development, `wasp start` still runs the client on port `3000` and the server on `3001`, but the browser only talks to `3000`: the client dev server proxies Wasp's routes (`/auth`, `/operations`, `/crud`, the new `/health`) and your `api` paths to the server.

Hosting the client separately from the server (the way it worked before, now called the [split deployment mode](./deployment/intro.md#deployment-modes)) still works, but it's no longer the default: opt into it with `deployment: { mode: "split" }` in `main.wasp.ts`.

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

If your final stage doesn't continue from Wasp's, it also needs the built client, which the server now serves:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```dockerfile title="Dockerfile"
    FROM node:24 AS production
    # ...
    COPY --from=server-builder /app/.wasp/out/server/bundle .wasp/out/server/bundle
    ENTRYPOINT ["npm", "run", "start-production"]
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```dockerfile title="Dockerfile"
    FROM node:24 AS production
    # ...
    COPY --from=server-builder /app/.wasp/out/server/bundle .wasp/out/server/bundle
    // highlight-next-line
    COPY web-app/build .wasp/out/web-app/build
    ENTRYPOINT ["npm", "run", "start-production"]
    ```
  </TabItem>
</Tabs>

Run `wasp dockerfile` to see the exact stage names and paths in your generated Dockerfile.

### 6. Update Fly database deployment flags

If you use database sizing options with `wasp deploy fly launch` or `wasp deploy fly create-db`, rename them as follows:

| Before                  | After                              |
| ----------------------- | ---------------------------------- |
| `--vm-size`             | `--db-vm-size`                     |
| `--initial-cluster-size` | `--db-initial-cluster-size`        |
| `--volume-size`         | `--db-volume-size`                 |

### 7. Pick your deployment mode

Your app now deploys as one app by default: the server serves the built client, and the browser talks to one origin. That's the [single deployment mode](./deployment/intro.md#deployment-modes), and it's what steps 9 to 13 walk you through.

The previous topology, the client hosted separately from the server, is still fully supported as the **split mode**. If that's what you want, take [step 8](#8-keep-hosting-the-client-separately) and you're done with this part of the migration.

### 8. Keep hosting the client separately

Only do this if you want to keep the client on a static host like Netlify or Cloudflare, separate from the server. That's the [split deployment mode](./deployment/intro.md#deployment-modes), the topology you already have. Opt into it in `main.wasp.ts`:

```ts title="main.wasp.ts"
export default app({
  // ...
  // highlight-next-line
  deployment: { mode: "split" },
});
```

Nothing else about your deployment changes: keep building the client yourself with `REACT_APP_API_URL=<server origin> npx vite build` after `wasp build`, keep `WASP_WEB_CLIENT_URL` pointed at the client's URL, and keep your OAuth redirect URIs on the server's origin. You can **skip steps 9 to 13** and go straight to [step 14](#14-update-hand-written-mocks-in-client-tests).

The [Netlify](./guides/deployment/cloud-providers/netlify.md) and [Cloudflare](./guides/deployment/cloud-providers/cloudflare.md) guides describe this setup.

### 9. Stop building the client separately

`wasp build` now builds the client into `.wasp/out/web-app/build` and the Dockerfile copies it into the image, so remove the `npx vite build` step from your deployment scripts and CI. Because the client is built by `wasp build`, your `REACT_APP_*` client env vars have to be in **its** environment (or in the environment of `wasp deploy`, which runs it):

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```yaml title=".github/workflows/deploy.yml"
    - run: wasp build
    # highlight-next-line
    - run: REACT_APP_API_URL=https://api.myapp.com REACT_APP_NAME="My App" npx vite build
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```yaml title=".github/workflows/deploy.yml"
    # highlight-next-line
    - run: REACT_APP_NAME="My App" wasp build
    ```
  </TabItem>
</Tabs>

`wasp build` needs your dependencies installed, so run `wasp install` first. `REACT_APP_API_URL` is gone: the client talks to its own origin. Keep it only if you host the client separately (see [step 8](#8-keep-hosting-the-client-separately)).

### 10. Update your server env vars

**If you use `wasp deploy fly` or `wasp deploy railway`, skip to [step 13](#13-upgrade-an-existing-wasp-deploy-project).**

`WASP_SERVER_URL` is now your app's public URL, the one users open in the browser. `WASP_WEB_CLIENT_URL` is optional and defaults to `WASP_SERVER_URL`, which is right when the server serves the client.

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```env title="Server env vars"
    WASP_SERVER_URL=https://api.myapp.com
    WASP_WEB_CLIENT_URL=https://myapp.com
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```env title="Server env vars"
    WASP_SERVER_URL=https://myapp.com
    ```
  </TabItem>
</Tabs>

Since there's only one app, point your domain at it and delete the client's static hosting once the new version is up. The [deployment guides](./deployment/deployment-methods/cloud-providers.md) have been updated for the single app.

### 11. Update your OAuth redirect URIs

If you use Google, GitHub, Discord, Keycloak, Microsoft or Slack auth, the redirect URI you registered with the provider now uses the app's origin (the one the browser talks to) instead of the server's. The path stays the same.

| Before                                       | After                                        |
| -------------------------------------------- | -------------------------------------------- |
| `http://localhost:3001/auth/google/callback` | `http://localhost:3000/auth/google/callback` |
| `https://api.myapp.com/auth/google/callback` | `https://myapp.com/auth/google/callback`     |

Do this for every provider and every environment (development, staging, production). The [social auth pages](./auth/social-auth/overview.md) have the updated values.

### 12. Update your health checks

`GET /` used to return an empty `200` on the server. That route is gone: in single deployment mode the client is served there, and in split mode `/` returns a `404` unless one of your `api`s claims it. Either way, if your host or reverse proxy has a health check, point it at `/health`, which returns `{"status":"ok"}`:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```caddyfile title="Caddyfile"
    reverse_proxy localhost:3001 {
      // highlight-next-line
      health_uri /
    }
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```caddyfile title="Caddyfile"
    reverse_proxy localhost:3001 {
      // highlight-next-line
      health_uri /health
    }
    ```
  </TabItem>
</Tabs>

### 13. Upgrade an existing `wasp deploy` project

`wasp deploy fly` and `wasp deploy railway` now deploy one app and no longer touch the client app or service they created earlier. They don't delete it either, so after your first deploy with 0.26:

1. Deploy as usual, for example `wasp deploy fly deploy` or `wasp deploy railway deploy <project-name>`.
1. Point `WASP_WEB_CLIENT_URL` at the server app, since that is where your app lives now. On Fly, run `wasp deploy fly cmd --context server secrets set WASP_WEB_CLIENT_URL=https://<app-name>-server.fly.dev`. On Railway, set it on the server service in the Railway dashboard.
1. Update your OAuth redirect URIs to the server app's URL, as in [step 11](#11-update-your-oauth-redirect-uris). Your app now lives at the server URL (for example `https://my-wasp-app-server.fly.dev`).
1. If you had a custom domain on the client app, move it to the server app (Fly: `wasp deploy fly cmd --context server certs create mycoolapp.com`; Railway: add the domain to the server service) and set `WASP_SERVER_URL` and `WASP_WEB_CLIENT_URL` to it.
1. Delete the old client app: `fly apps destroy <app-name>-client` on Fly, or remove the `<project-name>-client` service in the Railway dashboard. On Fly, also delete `fly-client.toml` from your project.

`--custom-server-url`, `--skip-client` and `--client-secret` only apply in split mode; Wasp prints a notice if you pass them otherwise.

### 14. Update hand-written mocks in client tests

`mockQuery` and `mockApi` from `wasp/client/test` keep working. So do hand-written `msw` handlers built on `config.apiUrl`, since it follows the client's own origin. Handlers with a hardcoded server URL need the API URL instead:

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

### 15. Enjoy your updated Wasp app

That's it!
