---
title: From 0.25 to 0.26
---

# Migration from 0.25 to 0.26

import InstallInstructions from './\_install-instructions.md'

<InstallInstructions version="0.26" />

## What's new in 0.26?

### Wasp now manages your app's dev URLs

`wasp start` now derives your app's dev URLs from the ports the client and the server run on, and tells both sides about them.

In development, setting the URLs manually in the env vars now fails.

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

### 3. Stop setting the dev URLs yourself

Wasp now derives your app's dev URLs from the ports it runs on, so it fails if you also set them.

Remove `WASP_SERVER_URL` and `WASP_WEB_CLIENT_URL` from your `.env.server` if you were setting them manually:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```env title=".env.server"
    # highlight-start
    WASP_SERVER_URL=http://localhost:3001
    WASP_WEB_CLIENT_URL=http://localhost:3000
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
    REACT_APP_API_URL=http://localhost:3001
    REACT_APP_NAME=My App
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```env title=".env.client"
    REACT_APP_NAME=My App
    ```
  </TabItem>
</Tabs>

Wasp fills in the URLs for you, so you no longer have to keep them in sync by hand.

:::info

Your deployed app still uses these environment variables, so don't remove them from your deploy configuration. Wasp only takes them over in development, where it is the one starting your app.
:::

### 4. Update your custom Dockerfile

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


### 4. Update Fly database deployment flags

If you use database sizing options with `wasp deploy fly launch` or `wasp deploy fly create-db`, rename them as follows:

| Before                  | After                              |
| ----------------------- | ---------------------------------- |
| `--vm-size`             | `--db-vm-size`                     |
| `--initial-cluster-size` | `--db-initial-cluster-size`        |
| `--volume-size`         | `--db-volume-size`                 |

### 5. Enjoy your updated Wasp app

That's it!
