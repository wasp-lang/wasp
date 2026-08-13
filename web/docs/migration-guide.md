---
title: From 0.25 to 0.26
---

# Migration from 0.25 to 0.26

import InstallInstructions from './\_install-instructions.md'

<InstallInstructions version="0.26" />

## What's new in 0.26?

**Your Wasp app is now one app.**

Until now, a Wasp app was two programs: a client that Vite built into static files and served on port 3000, and an Express server that ran on port 3001. They found each other through `REACT_APP_API_URL`, talked over CORS, and were deployed as two separate artifacts.

In 0.26 they are one [Nitro](https://nitro.build/) server that serves your pages, your prerendered routes, your API and your WebSockets. What that gets you:

- **One process in development.** `wasp start` runs a single Vite server on `http://localhost:3000`. Your server code hot-reloads the way your client code always did, instead of restarting the whole backend.
- **One artifact in production.** `wasp build` produces a single self-contained server, and a single Docker image to deploy. No more building and hosting your client separately.
- **One origin.** Your pages and your API share a URL, so `REACT_APP_API_URL` and CORS are no longer part of getting your app running.
- **A new WebSocket API.** Socket.IO is gone, replaced by Wasp's own layer over the platform's WebSockets.

Most of your app doesn't notice. Operations, CRUD, custom `api`s, `apiNamespace`s, middleware, all auth flows, jobs, emails, and `prerender` all work exactly as they did. The migration steps below cover the parts that do.

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

### 3. Rewrite your WebSocket code

_Skip this step if your app doesn't use `webSocket`._

Wasp's WebSockets no longer run on Socket.IO. **Your client code doesn't change**: `useSocket`, `useSocketListener` and `socket.emit` all keep working, and so do the event-map interfaces you typed them with. Your server code does.

Instead of a function that receives an `io` server and registers connection callbacks, your `webSocketFn` is now a definition you create with `defineWebSocket`: a set of hooks Wasp calls while a connection lives.

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```ts title="src/webSocket.ts"
    import { v4 as uuidv4 } from "uuid";
    import {
      type WaspSocketData,
      type WebSocketDefinition,
    } from "wasp/server/webSocket";

    export const webSocketFn: WebSocketFn = (io, context) => {
      io.on("connection", (socket) => {
        const username =
          socket.data.user?.getFirstProviderUserId() ?? "Unknown";
        console.log("a user connected: ", username);

        socket.on("chatMessage", async (msg) => {
          io.emit("chatMessage", { id: uuidv4(), username, text: msg });
        });
      });
    };

    type WebSocketFn = WebSocketDefinition<
      ClientToServerEvents,
      ServerToClientEvents,
      InterServerEvents,
      SocketData
    >;

    interface ServerToClientEvents {
      chatMessage: (msg: {
        id: string;
        username: string;
        text: string;
      }) => void;
    }
    interface ClientToServerEvents {
      chatMessage: (msg: string) => void;
    }
    interface InterServerEvents {}
    interface SocketData extends WaspSocketData {}
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```ts title="src/webSocket.ts"
    import { v4 as uuidv4 } from "uuid";
    import { broadcast, defineWebSocket } from "wasp/server/webSocket";

    export const webSocketFn = defineWebSocket<
      ClientToServerEvents,
      ServerToClientEvents
    >({
      open(peer) {
        const username = peer.data.user?.getFirstProviderUserId() ?? "Unknown";
        console.log("a user connected: ", username);
      },

      events: {
        async chatMessage(peer, msg) {
          const username =
            peer.data.user?.getFirstProviderUserId() ?? "Unknown";
          broadcast("chatMessage", { id: uuidv4(), username, text: msg });
        },
      },
    });

    interface ServerToClientEvents {
      chatMessage: (msg: {
        id: string;
        username: string;
        text: string;
      }) => void;
    }
    interface ClientToServerEvents {
      chatMessage: (msg: string) => void;
    }
    ```
  </TabItem>
</Tabs>

Here's how the pieces translate:

| Before (Socket.IO)                    | After                                             |
| ------------------------------------- | ------------------------------------------------- |
| `io.on("connection", (socket) => ...)` | the `open(peer, context)` hook                    |
| `socket.on("event", handler)`         | an entry in the `events` map                      |
| `socket.on("disconnect", handler)`    | the `close(peer, details, context)` hook          |
| `socket.data.user`                    | `peer.data.user`                                  |
| `socket.emit(event, payload)`         | `peer.send(event, payload)`                       |
| `io.emit(event, payload)`             | `broadcast(event, payload)`                       |
| `socket.join(room)`                   | `peer.subscribe(topic)`                           |
| `io.to(room).emit(event, payload)`    | `publish(topic, event, payload)`                  |
| `socket.broadcast.to(room).emit(...)` | `peer.publishToOthers(topic, event, payload)`     |
| `WebSocketDefinition<C2S, S2C, I, D>` | `defineWebSocket<C2S, S2C>({ ... })`              |

A few things to know while you translate:

- **The `InterServerEvents` and `SocketData` type parameters are gone.** `defineWebSocket` takes two: what the client sends you, and what you send the client.
- **Every event carries exactly one payload.** An event declared as `(a: A, b: B) => void` only carries `a`. If you have multi-argument events, make them carry one object instead. TypeScript will point you at every call site.
- **`broadcast` and `publish` are plain functions**, so you can now send events from a Query, an Action or a Job, not just from a connection's hooks.
- **`publish` includes everybody in the topic.** `peer.publishToOthers` is the one that excludes the sender, matching Socket.IO's `socket.broadcast.to(room)`.
- **You can refuse a connection** by throwing a `Response` from the new `upgrade` hook. Wasp resolves the logged-in user before it runs, so `peer.data.user` is already there.
- **`socket.io` and `socket.io-client` are no longer installed.** If you imported them directly (for example to use namespaces), that code needs rewriting too. See the [WebSocket Channels guide](./guides/integrations/websocket-namespaces.md) for the topic-based replacement.

Read the full API in the [Web Sockets docs](./advanced/web-sockets.md).

### 4. Stop using `server` in your setup function

_Skip this step if your app doesn't have a server `setupFn`, or if it only uses `app`._

Wasp doesn't own an HTTP server anymore, so the `server` your setup function receives is no longer a real one. It still type-checks, but reading anything off it throws an error explaining this, so you'll find out the first time your app starts.

The `app` (your Express app) is unchanged, so setup functions that add routes or middleware keep working as they are:

```ts title="src/serverSetup.ts"
import { type ServerSetupFn } from "wasp/server";

export const setup: ServerSetupFn = async ({ app }) => {
  app.get("/customRoute", (_req, res) => {
    res.send("I am a custom route");
  });
};
```

If you used `server` to attach a WebSocket server of your own, use Wasp's [WebSocket support](./advanced/web-sockets.md) instead. If you used it for something else, come tell us on [Discord](https://discord.gg/rzdnErX) so we can find you a replacement.

One more thing about setup functions: they now run once when your app's server starts, and Wasp doesn't re-run them on every code change. If you edit your setup function, restart `wasp start` for the change to take effect.

### 5. Simplify your environment variables

Your app is served from one origin now, so the variables that used to point its two halves at each other mostly go away.

- **`REACT_APP_API_URL` is no longer required.** Your pages look for your API on their own origin by default, in development and in production alike. Remove it from your `.env.client` and from your production build unless you deliberately serve your API from another origin.
- **`WASP_SERVER_URL` is your app's public URL.** It is what Wasp builds links from: the ones in the emails it sends, and the ones it redirects OAuth logins to.
- **`WASP_WEB_CLIENT_URL` defaults to `WASP_SERVER_URL`.** Set it only when your pages really are somewhere else.
- **In development, both default to `http://localhost:3000`.** If you changed your Vite dev server's port, set both to that port in `.env.server`.
- **`PORT` in `.env.server` no longer moves your dev server.** Your app's development port comes from your Vite config. `PORT` is still what your *built* app listens on (`3001` by default), so your deployment configuration doesn't change.

If you registered redirect URIs with an OAuth provider for local development, update them from port 3001 to port 3000:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```
    http://localhost:3001/auth/google/callback
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```
    http://localhost:3000/auth/google/callback
    ```
  </TabItem>
</Tabs>

### 6. Update your deployment

`wasp build` now produces a single Docker image that serves your whole app.

- **Deploy one thing.** Whatever you used to host your client on (a static host, a CDN bucket, a `gostatic` or Caddy container) is no longer part of your deployment. Point your domain at your app, and retire the static host once your users are on it.
- **Your app listens on `PORT`** (`3001` by default), and serves everything from it: pages, assets, API, and WebSockets.
- **Client env vars are baked into the image.** Anything `REACT_APP_*` is written into your pages and assets while they are built, so it has to be there when the image is built, not when it runs. Pass it with the `WASP_CLIENT_ENV` build argument:

  ```shell
  docker build \
    --build-arg WASP_CLIENT_ENV="REACT_APP_EXAMPLE='value'" \
    -t my-wasp-app \
    .wasp/out
  ```

- **Health checks move.** `/` is one of your pages now, so it no longer answers with a bare `200`. Point your platform's health check at `/_wasp/health` instead.

If you are using `wasp deploy`:

- `wasp deploy fly` and `wasp deploy railway` now set up and deploy a single app. Run `wasp deploy <provider> setup` again to have Wasp reconfigure it.
- Your old client app (`<app>-client` on Fly, the `-client` service on Railway) stops receiving deployments. Wasp tells you about it when it finds one. Destroy it once your users are on your app's own URL, and delete the `fly-client.toml` file if you have one.
- `--client-secret`, `--skip-client` and `--custom-server-url` are ignored. Client env vars are part of the build now, and there is no separate client to deploy or to point at a server.

:::caution `wasp deploy` and client env vars
Your provider builds your app's image, and `wasp deploy` has no way of passing your `REACT_APP_*` variables to that build yet. If your app has any, build and push the image yourself for now, with the `WASP_CLIENT_ENV` build argument shown above.
:::

### 7. Update your custom Dockerfile

_Skip this step if you don't have a `Dockerfile` in your project's root._

If you are using a [custom Dockerfile](./deployment/deployment-methods/overview#customizing-the-dockerfile), you'll have to add one new line to it:

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

Wasp's own Dockerfile stages were also renamed, since they build your whole app now and not just its server. If your Dockerfile continues from one of them, rename it:

| Before             | After        |
| ------------------ | ------------ |
| `server-builder`    | `builder`    |
| `server-production` | `production` |

### 8. Check the smaller changes

These are unlikely to affect you, but they're worth a look:

- **A custom `api` no longer shadows a page.** If a request reaches a custom `api` path and your handler doesn't answer it, it now falls through to your app's pages instead of returning a 404. Requests to `/auth`, `/operations` and `/crud` paths that match nothing still get a JSON 404.
- **Errors that aren't `HttpError`s come back as JSON.** They used to be Express's HTML error page. `HttpError` responses are unchanged.
- **`npm run bundle` is gone**, along with `rollup`, `nodemon` and `dotenv`, from the generated server. If your scripts called it, they should call `npx vite build` (or just `docker build`) instead.
- **Editing your `vite.config.ts` needs a `wasp start` restart**, the same way editing your setup function does.

### 9. Enjoy your updated Wasp app

That's it!
