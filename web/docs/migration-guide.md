---
sidebar_label: From 0.26 to 0.27
---

# Migration from 0.26 to 0.27

import InstallInstructions from './_install-instructions.md'

<InstallInstructions version="0.27" />

## What's new in 0.27?

### Prisma 6

Wasp now uses Prisma 6. Check out Prisma's [upgrade guide](https://www.prisma.io/docs/orm/more/upgrade-guides/upgrading-versions/upgrading-to-prisma-6) for the full list of changes.

## How to migrate?

### 1. Bump the Wasp version

Update the version field in your Wasp config to `^0.27.0`.

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```ts title="main.wasp.ts"
    export default app({
      // highlight-next-line
      wasp: { version: "^0.26.0" },
      // ...
    });
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```ts title="main.wasp.ts"
    export default app({
      // highlight-next-line
      wasp: { version: "^0.27.0" },
      // ...
    });
    ```
  </TabItem>
</Tabs>

And run the following command to update the Wasp libraries in your project:

```bash
wasp install
```

### 2. Upgrade to Prisma 6

Update the `prisma` version in your `package.json`:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```json title="package.json"
    {
      "devDependencies": {
        // highlight-next-line
        "prisma": "5.19.1"
      }
    }
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```json title="package.json"
    {
      "devDependencies": {
        // highlight-next-line
        "prisma": "6.19.3"
      }
    }
    ```
  </TabItem>
</Tabs>

If you also list `@prisma/client` in your `package.json`, update it to the same version. Then run `wasp install`.

Prisma 6 changed how some things are stored in the database, so run `wasp db migrate-dev` to check if your app needs a new migration. Prisma 6 also has a few breaking changes in Prisma Client. Check [Prisma's upgrade guide](https://www.prisma.io/docs/orm/more/upgrade-guides/upgrading-versions/upgrading-to-prisma-6) to see if any of them affect your app.

### 3. Import `HttpError` from `wasp/errors`

The server and the client now use the same `HttpError` class, so it moved from `wasp/server` to `wasp/errors`. Update your imports:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```ts title="src/queries.ts"
    import { HttpError } from "wasp/server"
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```ts title="src/queries.ts"
    // highlight-next-line
    import { HttpError } from "wasp/errors"
    ```
  </TabItem>
</Tabs>

### 4. Read `HttpError` data directly on the client

On the client, failed operations and auth actions now throw an `HttpError`, the same class you throw on the server.
Its `data` is now the `data` you passed on the server.
Before, `data` held the whole response body, so you had to read `error.data.data`.

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```ts title="src/SignupPage.tsx"
    try {
      await signup({ username, password })
    } catch (error: any) {
      if (error.statusCode === 422) {
        setErrorMessage(error.data.data.message)
      }
    }
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```ts title="src/SignupPage.tsx"
    // highlight-next-line
    import { HttpError } from "wasp/errors"

    try {
      await signup({ username, password })
    } catch (error: unknown) {
      // highlight-next-line
      if (error instanceof HttpError && error.statusCode === 422) {
        // highlight-next-line
        setErrorMessage(String(error.data?.message))
      }
    }
    ```
  </TabItem>
</Tabs>

### 5. Enjoy your updated Wasp app

That's it!
