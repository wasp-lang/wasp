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

### TypeScript bumped to 6.0.3

Wasp 0.24 ships with TypeScript 6.0.3. TS 6 deprecates several legacy `tsconfig.json` options (e.g., `moduleResolution: "node"`, `outFile`, `downlevelIteration`) that will become errors in TypeScript 7, and it changes the default for `types` from "auto-include all `@types/*`" to `[]`. You now need to list the type packages your code relies on explicitly. Wasp 0.24's required `tsconfig.src.json` shape adds `"types": ["react", "react-dom"]` to reflect this. If you have additional customizations to your project's `tsconfig.*.json`, review the [TypeScript 6 release notes](https://devblogs.microsoft.com/typescript/announcing-typescript-6-0/) for any other options you may need to update.

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

### 2. Update TypeScript to 6.0.3

Wasp 0.24 requires TypeScript 6.0.3. Update it in your `package.json`:

<Tabs>
<TabItem value="before" label="Before">

```json title="package.json"
{
  "devDependencies": {
    "typescript": "5.9.3"
  }
}
```

</TabItem>
<TabItem value="after" label="After">

```json title="package.json"
{
  "devDependencies": {
    "typescript": "6.0.3"
  }
}
```

</TabItem>
</Tabs>

Then add `"types": ["react", "react-dom"]` to your `tsconfig.src.json`'s `compilerOptions` (and `"types": ["node"]` to your `tsconfig.wasp.json` if you have a TypeScript-based Wasp config file). TypeScript 6 no longer auto-includes `@types/*` packages from `node_modules`, so Wasp now requires these to be listed explicitly:

<Tabs>
<TabItem value="before-tsconfig" label="Before">

```json title="tsconfig.src.json"
{
  "compilerOptions": {
    "lib": ["dom", "dom.iterable", "esnext"]
    // ...
  }
}
```

```json title="tsconfig.wasp.json"
{
  "compilerOptions": {
    "lib": ["ES2023"]
    // ...
  }
}
```

</TabItem>
<TabItem value="after-tsconfig" label="After">

```json title="tsconfig.src.json"
{
  "compilerOptions": {
    "lib": ["dom", "dom.iterable", "esnext"],
    "types": ["react", "react-dom"]
    // ...
  }
}
```

```json title="tsconfig.wasp.json"
{
  "compilerOptions": {
    "lib": ["ES2023"],
    "types": ["node"]
    // ...
  }
}
```

</TabItem>
</Tabs>

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
