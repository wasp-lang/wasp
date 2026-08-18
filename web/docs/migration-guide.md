---
title: From 0.25 to 0.26
---

# Migration from 0.25 to 0.26

import InstallInstructions from './\_install-instructions.md'

<InstallInstructions version="0.26" />

## What's new in 0.26?

<!-- TBA -->

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


### 3. Update your custom Dockerfile

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

### 5. NFC normalization for email and username identifiers

Wasp now normalizes email and username identifiers to prevent issues with different Unicode encodings for identical-looking characters. 

If all your users have standard English characters (ASCII) for their emails and usernames, you can skip this step.

If your database contains non-ASCII characters (e.g. `ü`), some users may not be able to log in after the upgrade. To fix this, you must run a one-time script to normalize existing database records. Here is the script:

```ts title="scripts/nfc-backfill.ts"
import { PrismaClient } from "@prisma/client";

const prisma = new PrismaClient();

const rows = await prisma.authIdentity.findMany({
  where: { providerName: { in: ["email", "username"] } },
});

let rewritten = 0;
const collisions: string[] = [];

for (const row of rows) {
  const target = row.providerUserId.normalize("NFC").toLowerCase();
  if (target === row.providerUserId) continue;
  try {
    await prisma.authIdentity.update({
      where: {
        providerName_providerUserId: {
          providerName: row.providerName,
          providerUserId: row.providerUserId,
        },
      },
      data: { providerUserId: target },
    });
    rewritten += 1;
  } catch (e) {
    // Two existing rows normalize to the same NFC string, indicating a duplicate account.
    collisions.push(`${row.providerName}:${row.providerUserId} -> ${target}`);
  }
}

console.log(`NFC rewrite done. ${rewritten} rows updated.`);
if (collisions.length) {
  console.warn(
    `${collisions.length} collision(s) need manual reconciliation:\n` +
      collisions.join("\n"),
  );
}
```

Run it once after deploying the new server:

```bash
npx tsx scripts/nfc-backfill.ts
```

**Collisions.** If two existing rows normalize to the same NFC string (e.g. one user signed up on a device that emitted NFC and another on one that emitted NFD), the script logs them but leaves them in place. Reconcile by hand, typically by deleting the duplicate account or migrating one of them to a new identifier.

### 6. Enjoy your updated Wasp app

That's it!
