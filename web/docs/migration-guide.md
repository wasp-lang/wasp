---
title: From 0.25 to 0.26
---

# Migration from 0.25 to 0.26

import InstallInstructions from './\_install-instructions.md'

<InstallInstructions version="0.26" />

## What's new in 0.26?

### Wasp picks the dev ports

`wasp start` now decides which ports the client and the server run on. If the default ports (3000 and 3001) are taken, it moves the app to free ones instead of failing, so you can run several Wasp apps side by side. You can also pick the ports yourself:

```bash
wasp start --client-port 4000 --server-port 4001
```

Because Wasp needs to tell each side where the other one is running, it has to be the one choosing the ports, and the URLs that follow from them. Setting any of those yourself in development now fails with an error that points you to these options. In production nothing changes: you still set the URLs and the port yourself.

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

### 2. Stop setting the dev ports and URLs yourself

Wasp now picks the ports your app runs on in development and derives its URLs from them, so it fails if you also set them.

If your `vite.config.ts` sets `server.port` (or `server.strictPort`), remove it:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```ts title="vite.config.ts"
    export default defineConfig({
      plugins: [wasp()],
      // highlight-start
      server: {
        port: 4000,
      },
      // highlight-end
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

Then remove `PORT`, `WASP_SERVER_URL` and `WASP_WEB_CLIENT_URL` from your `.env.server`:

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

And `REACT_APP_API_URL` from your `.env.client`:

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

To keep running on those ports, pass them to `wasp start` instead:

```bash
wasp start --client-port 4000 --server-port 4001
```

Wasp fills in the URLs for you from the ports it picked, so you no longer have to keep them in sync by hand.

:::info Production is unaffected
These env vars still work the same way when you deploy. Wasp only takes them over in development, where it is the one starting your app.
:::

### 3. Enjoy your updated Wasp app

That's it!
