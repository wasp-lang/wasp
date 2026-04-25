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

### 3. Update your `tsconfig.json` / `tsconfig.src.json` file

Due to internal `wasp/sdk` package changes, we require a simple change to your `tsconfig.json` or `tsconfig.src.json` if you are using the Wasp TS configuration.

Update the `include` field of the TypeScript configuration:

```json title="tsconfig.json"
{
  "compilerOptions": {
    // ...
    "outDir": ".wasp/out/user"
  },
  // highlight-next-line
  "include": ["src", ".wasp/out/types"]
}

```

### 4. Enjoy your updated Wasp app

That's it!
