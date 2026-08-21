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

### 7. Normalize existing email and username identifiers

Wasp now stores and looks up `email` and `username` identifiers in their [NFC](https://unicode.org/reports/tr15/) form, so identifiers that look the same but use different Unicode encodings (e.g. `ü` written as a single character vs. `u` followed by a combining diaeresis) resolve to the same identity.

If all your users' emails and usernames use only ASCII characters, you can skip this step.

Otherwise, identifiers already stored in a decomposed form no longer match the logins coming from the new server, so you have to rewrite them once. Add a seed function that does it:

```ts title="src/nfcBackfill.ts"
import { type DbSeedFn } from "wasp/server";

export const nfcBackfill: DbSeedFn = async (prisma) => {
  const identities = await prisma.authIdentity.findMany({
    where: { providerName: { in: ["email", "username"] } },
  });

  const collisions: string[] = [];

  for (const identity of identities) {
    const normalizedId = identity.providerUserId.normalize("NFC").toLowerCase();
    if (normalizedId === identity.providerUserId) {
      continue;
    }

    try {
      await prisma.authIdentity.update({
        where: {
          providerName_providerUserId: {
            providerName: identity.providerName,
            providerUserId: identity.providerUserId,
          },
        },
        data: { providerUserId: normalizedId },
      });
    } catch (error) {
      if (!isUniqueConstraintViolation(error)) {
        throw error;
      }
      collisions.push(`${identity.providerName}:${identity.providerUserId}`);
    }
  }

  if (collisions.length > 0) {
    throw new Error(
      `${collisions.length} identity(ies) need manual reconciliation:\n${collisions.join("\n")}`,
    );
  }
};

function isUniqueConstraintViolation(error: unknown): boolean {
  return (
    typeof error === "object" &&
    error !== null &&
    "code" in error &&
    error.code === "P2002"
  );
}
```

Register it in your app config:

```ts title="main.wasp.ts"
import { nfcBackfill } from "./src/nfcBackfill" with { type: "ref" }

export default app({
  // ...
  db: {
    seeds: [nfcBackfill],
  },
})
```

Then run it against your production database:

```bash
DATABASE_URL=<production-db-url> wasp db seed nfcBackfill
```

:::caution Run it while your app is not serving auth requests
Run the backfill in a maintenance window: stop the old server (or block signups and logins) first, and deploy the new server only once the backfill finishes.

If both an old server and decomposed identities are live at the same time, a user whose identity hasn't been rewritten yet can sign up again under the normalized identifier, which leaves you with two accounts for the same person.
:::

If two identities normalize to the same value (e.g. one user signed up from a device that sent NFC and another from a device that sent NFD), the backfill leaves them untouched and fails with the list of identities to reconcile. Fix them by hand, usually by deleting the duplicate account or by moving one of them to a new identifier, and then run the backfill again.

### 8. Enjoy your updated Wasp app

That's it!
