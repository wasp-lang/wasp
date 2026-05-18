---
title: Migration from 0.23.X to 0.24.X
---

import InstallInstructions from './_install-instructions.md'
import LegacyInstallerMigration from './_legacy_installer_migration.md'

<LegacyInstallerMigration />
<InstallInstructions version="0.24" />

## What's new in 0.24.X?

### Replaced Axios with [ky](https://github.com/sindresorhus/ky) in the client API module

The `api` export from `wasp/client/api` is now a [ky](https://github.com/sindresorhus/ky) instance instead of Axios. Ky is a tiny HTTP client built on `fetch` that provides a cleaner API with method shortcuts, automatic JSON handling, and hooks.

### The Wasp TS config is now called the Wasp Spec

The Wasp TS config (`main.wasp.ts`) is now called the Wasp Spec, and it should import from `@wasp.sh/spec` instead of `wasp-config`. This better reflects the purpose of this file as a place to configure and customize Wasp's behavior in your project.

### Real JS imports in Wasp TS spec

In `main.wasp.ts`, you can now use real JS imports and pass imported values directly to Wasp declarations instead of using import objects like `{ import, from }`.

### Stricter validation for Wasp TS projects

Wasp now validates more of the TS spec support files. If your project uses `main.wasp.ts`, Wasp checks that `package.json` includes the required `@types/node` dev dependency and that `tsconfig.wasp.json` includes the required `@src/*` path mapping.

## How to migrate?

### 1. Bump the Wasp version

Update the version field in your Wasp config to `^0.24.0`:

```wasp title="main.wasp"
app MyApp {
  wasp: {
    version: "^0.24.0"
  },
  // ...
}
```

### 2. Update client code that uses `api` from `wasp/client/api`

**If you don't use the `api` function from `wasp/client/api` directly, you can skip this step.**

The `api` object was previously an Axios instance. It is now a [ky](https://github.com/sindresorhus/ky) instance with a pre-configured base URL and authentication. Update your code as follows:

<Tabs>
  <TabItem value="before" label="Before">
    ```ts
    import { api } from 'wasp/client/api'

    // Making requests
    const response = await api.get('/foo/bar')
    const data = response.data

    // POST with body
    await api.post('/foo/bar', { key: 'value' })

    // Error handling
    import { type AxiosError } from 'axios'
    try {
      await api.get('/foo/bar')
    } catch (e) {
      const error = e as AxiosError
      console.log(error.response?.status)
    }
    ```
  </TabItem>

  <TabItem value="after" label="After">
    ```ts
    import { api } from 'wasp/client/api'
    import { isHTTPError } from 'ky'

    // Making requests
    const data = await api.get('/foo/bar').json()

    // POST with body
    await api.post('/foo/bar', { json: { key: 'value' } })

    // Error handling
    try {
      await api.get('/foo/bar').json()
    } catch (e) {
      if (isHTTPError(e)) {
        console.log(e.response.status)
      }
    }
    ```
  </TabItem>
</Tabs>

You can also remove `axios` from your project's dependencies if you added it only for use with the Wasp `api` wrapper.

### 3. Update Wasp TS spec support files

Make sure your `package.json` includes `@types/node` in `devDependencies`, and update the `wasp-config` dependency:

<Tabs>
  <TabItem value="before" label="Before">
    ```json title="package.json"
    {
      "devDependencies": {
        // highlight-next-line
        "wasp-config": "file:.wasp/wasp-config"
      }
    }
    ```
  </TabItem>

  <TabItem value="after" label="After">
    ```json title="package.json"
    {
      "devDependencies": {
        // highlight-next-line
        "@types/node": "^24.0.0",
        // highlight-next-line
        "@wasp.sh/spec": "file:.wasp/spec"
      }
    }
    ```
  </TabItem>
</Tabs>

Make sure your `tsconfig.wasp.json` includes the `@src/*` path mapping:

```json title="tsconfig.wasp.json"
{
  "compilerOptions": {
    // ...
    "paths": {
      // highlight-next-line
      "@src/*": ["./src/*"]
    }
  }
}
```

And finally, update your `main.wasp.ts` to import from `@wasp.sh/spec` instead of `wasp-config`:


<Tabs>
  <TabItem value="before" label="Before">
    ```ts title="main.wasp.ts"
    // highlight-next-line
    import { ActionConfig, App, ExtImport } from "wasp-config";

    const app = new App({
      // ...
    });
    ```
  </TabItem>

  <TabItem value="after" label="After">
    ```ts title="main.wasp.ts"
    // highlight-next-line
    import { ActionConfig, App, ExtImport } from "@wasp.sh/spec";

    const app = new App({
      // ...
    });
    ```
  </TabItem>
</Tabs>

### 4. Enjoy your updated Wasp app

That's it!
