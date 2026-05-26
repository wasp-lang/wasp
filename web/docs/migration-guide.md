---
title: Migration from 0.23.X to 0.24.X
---

import InstallInstructions from './_install-instructions.md'
import LegacyInstallerMigration from './_legacy_installer_migration.md'

<LegacyInstallerMigration />
<InstallInstructions version="0.24" />

## What's new in 0.24.X?

### Wasp Spec is the new way to configure apps

You now configure your app with a Wasp Spec file, `main.wasp.ts`. It has a new syntax that is incompatible with both the old Wasp DSL and the Wasp TS config. The new Wasp Spec is more flexible and powerful, allowing you to use JS imports, functions, and values in your app configuration. You can read more about the new Wasp Spec in the new [Wasp Spec documentation](./general/spec.md).

#### The Wasp Spec package is now `@wasp.sh/spec`

The Wasp TS config package (`wasp-config`) is now the Wasp Spec package (`@wasp.sh/spec`). This better reflects the purpose of this package as the API for configuring and customizing Wasp's behavior in your project.

#### Reference imports in the Wasp Spec

In `main.wasp.ts`, you can now import app source references with `with { type: "ref" }` and pass imported values directly to Wasp declarations instead of using import objects like `{ import, from }`.

```ts title="main.wasp.ts"
import MainPage from './src/MainPage' with { type: 'ref' }
import { getTasks } from './src/operations' with { type: 'ref' }
```

### Client tests now use your project's Vitest package

Wasp now expects `vitest` to be in your project's `devDependencies` because `wasp test client` runs the Vitest package installed in your project.

### Client API module now uses [ky](https://github.com/sindresorhus/ky) instead of Axios

The `api` export from `wasp/client/api` is now a [ky](https://github.com/sindresorhus/ky) instance instead of Axios. Ky is a tiny HTTP client built on `fetch` that provides a cleaner API with method shortcuts, automatic JSON handling, and hooks.

## How to migrate?

### 1. Bump the Wasp version

When rewriting your config as a Wasp Spec, set the version field to `^0.24.0`:

```ts title="main.wasp.ts"
import { app } from '@wasp.sh/spec'

export default app({
  name: 'MyApp',
  wasp: { version: '^0.24.0' },
  // ...
  parts: [],
})
```

### 2. Migrate your `main.wasp` or `main.wasp.ts` to the new Wasp Spec

- If you were using a `main.wasp` file, follow [our guide on how to migrate from the Wasp DSL to the new Wasp Spec](./guides/legacy/wasp-dsl.md).
- If you were using a `main.wasp.ts` file, follow [our guide on how to migrate from the old Wasp TS config to the new Wasp Spec](./guides/legacy/wasp-ts-config.md).

### 3. Add `vitest` to your `package.json`

Add `vitest` to `devDependencies` because `wasp test client` now runs the Vitest package installed in your project.

```json title="package.json"
{
  "devDependencies": {
    // highlight-next-line
    "vitest": "^4.0.16"
  }
}
```

### 4. Update client code that uses `api` from `wasp/client/api`

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

### 5. Enjoy your updated Wasp app

That's it!
