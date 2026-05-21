---
title: Migration from 0.23.X to 0.24.X
---

import InstallInstructions from './_install-instructions.md'
import LegacyInstallerMigration from './_legacy_installer_migration.md'

<LegacyInstallerMigration />
<InstallInstructions version="0.24" />

## What's new in 0.24.X?

### New Wasp Spec, removed the old Wasp DSL and Wasp TS config

You now configure your app with Wasp Spec file, `main.wasp.ts`. It has a new syntax that is incompatible with both the old Wasp DSL and the Wasp TS config. The new Wasp Spec is more flexible and powerful, allowing you to use real JS imports, functions, and values in your app configuration. You can read more about the new Wasp Spec in the new [Wasp Spec documentation](../general/spec.md).

### Replaced Axios with [ky](https://github.com/sindresorhus/ky) in the client API module

The `api` export from `wasp/client/api` is now a [ky](https://github.com/sindresorhus/ky) instance instead of Axios. Ky is a tiny HTTP client built on `fetch` that provides a cleaner API with method shortcuts, automatic JSON handling, and hooks.

### Stricter validation for Wasp TS projects

Wasp now validates more of the TS spec support files. If your project uses `main.wasp.ts`, Wasp checks that `package.json` includes the required `@types/node` dev dependency, that `tsconfig.wasp.json` includes the required TS spec options, and that `tsconfig.src.json` excludes Wasp TS spec files.
Wasp now validates more of the TS spec support files. If your project uses `main.wasp.ts`, Wasp checks that `package.json` includes the required `@types/node` dev dependency, that `tsconfig.wasp.json` includes the required TS spec options, and that `tsconfig.src.json` excludes Wasp TS spec files.

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

### 2. Migrate your `main.wasp` or `main.wasp.ts` to the new Wasp Spec

- If you were using a `main.wasp` file, follow [our guide on how to migrate from the Wasp DSL to the new Wasp Spec](../guides/legacy/wasp-dsl.md).
- If you were using a `main.wasp.ts` file, follow [our guide on how to migrate from the old Wasp TS config to the new Wasp Spec](../guides/legacy/wasp-ts-config.md).

### 3. Update client code that uses `api` from `wasp/client/api`

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

### 4. Enjoy your updated Wasp app

That's it!
