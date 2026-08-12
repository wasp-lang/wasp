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


### 4. Check for duplicate auth identities (only if you wrote them manually)

Wasp now enforces that an account has at most one identity per auth provider, by adding a `@@unique([authId, providerName])` constraint to the [`AuthIdentity` entity](/auth/entities/entities.md#entities-explained). The database migration that adds this constraint **will fail if your database contains duplicates**.

No built-in Wasp flow can create a duplicate, so you only need to check this if your app writes `AuthIdentity` rows directly through raw Prisma or custom SQL insertions. If it doesn't, you're good to go!

To find duplicates, run this query against your database:

```sql
SELECT "authId", "providerName", COUNT(*)
FROM "AuthIdentity" GROUP BY 1, 2 HAVING COUNT(*) > 1;
```

If it returns rows, decide which identity to keep for each `(authId, providerName)` pair and delete the rest before migrating.

### 5. Enjoy your updated Wasp app

That's it!
