---
title: Migration from 0.23.X to 0.24.X
---

import InstallInstructions from './\_install-instructions.md'
import LegacyInstallerMigration from './\_legacy_installer_migration.md'
import LLMPrompt from '@site/src/components/LLMPrompt'

<LegacyInstallerMigration />
<InstallInstructions version="0.24" />

## What's new in 0.24.X?

### Wasp Spec is the new way to configure apps

You now configure your app with a Wasp Spec file, `main.wasp.ts`. It has a new syntax that is incompatible with both the old Wasp DSL and the Wasp TS config. The new Wasp Spec is more flexible and powerful, allowing you to use JS imports, functions, and values in your app configuration. You can read more about the new Wasp Spec in the new [Wasp Spec documentation](./general/spec.md).

#### The Wasp Spec package is now `@wasp.sh/spec`

The Wasp TS config package (`wasp-config`) is now the Wasp Spec package (`@wasp.sh/spec`). This better reflects the purpose of this package as the API for configuring and customizing Wasp's behavior in your project.

#### Reference imports in the Wasp Spec

In `main.wasp.ts`, you can now import app source references with `with { type: "ref" }` and pass imported values directly to Wasp Spec instead of using import objects like `{ import, from }`.

```ts title="main.wasp.ts"
import MainPage from "./src/MainPage" with { type: "ref" };
import { getTasks } from "./src/operations" with { type: "ref" };
```

### Client tests now use your project's Vitest package

Wasp now expects `vitest` to be in your project's `devDependencies` because `wasp test client` runs the Vitest package installed in your project.

### Client API module now uses [ky](https://github.com/sindresorhus/ky) instead of Axios

The `api` export from `wasp/client/api` is now a [ky](https://github.com/sindresorhus/ky) instance instead of Axios. Ky is a tiny HTTP client built on `fetch` that provides a cleaner API with method shortcuts, automatic JSON handling, and hooks.

## How to migrate?

### Use an agent to do it for you

Pick the prompt that matches your current config style and give it to your agent.

<Tabs>
  <TabItem value="dsl" label="Wasp DSL">

    If your app has a `main.wasp` file, use this prompt.

    <LLMPrompt label="LLM-assisted Wasp DSL migration">
{`
You are migrating my Wasp app from Wasp 0.23 to Wasp 0.24.

My app currently uses the Wasp DSL in main.wasp. Please convert the config to the Wasp Spec in \`main.wasp.ts\`.

Use these docs:

- 0.23 to 0.24 migration guide: https://raw.githubusercontent.com/wasp-lang/wasp/refs/heads/release/web/versioned_docs/version-0.24/migration-guide.md
- Wasp DSL to Wasp Spec conversion guide: https://raw.githubusercontent.com/wasp-lang/wasp/refs/heads/release/web/versioned_docs/version-0.24/guides/legacy/wasp-dsl.md
- Wasp Spec docs: https://raw.githubusercontent.com/wasp-lang/wasp/refs/heads/release/web/versioned_docs/version-0.24/general/spec.md
- Wasp Spec API constructors: https://raw.githubusercontent.com/wasp-lang/wasp/refs/heads/release/waspc/data/packages/spec/src/spec/publicApi/constructors.ts
- Wasp Spec API types: https://raw.githubusercontent.com/wasp-lang/wasp/refs/heads/release/waspc/data/packages/spec/src/spec/publicApi/tsAppSpec.ts

Important:

- Use the Wasp DSL conversion guide for the config conversion.
- After converting the config, return to the 0.23 to 0.24 migration guide and finish the shared migration steps.
- Keep the app's behavior the same.
- Use reference imports with \`with { type: "ref" }\` when importing components and functions from src.
- If splitting the spec into multiple files, export \`Spec\` from feature spec files and join them into \`main.wasp.ts\`.

Please make the changes directly in the repo and tell me what commands I should run to verify the migration.
`}
    </LLMPrompt>
  </TabItem>
  <TabItem value="ts-config" label="Wasp TS Config">

    If your app already has a `main.wasp.ts` file using the old class-based `new App(...)` API, use this prompt.

    <LLMPrompt label="LLM-assisted Wasp TS Config migration">
{`
You are migrating my Wasp app from Wasp 0.23 to Wasp 0.24.

My app currently uses the old class-based Wasp TS Config in \`main.wasp.ts\`. Please convert it to the new function-based Wasp Spec.

Use these docs:

- 0.23 to 0.24 migration guide: https://raw.githubusercontent.com/wasp-lang/wasp/refs/heads/release/web/versioned_docs/version-0.24/migration-guide.md
- Wasp TS Config to Wasp Spec conversion guide: https://raw.githubusercontent.com/wasp-lang/wasp/refs/heads/release/web/versioned_docs/version-0.24/guides/legacy/wasp-ts-config.md
- Wasp Spec docs: https://raw.githubusercontent.com/wasp-lang/wasp/refs/heads/release/web/versioned_docs/version-0.24/general/spec.md
- Wasp Spec API constructors: https://raw.githubusercontent.com/wasp-lang/wasp/refs/heads/release/waspc/data/packages/spec/src/spec/publicApi/constructors.ts
- Wasp Spec API types: https://raw.githubusercontent.com/wasp-lang/wasp/refs/heads/release/waspc/data/packages/spec/src/spec/publicApi/waspSpec.ts

Important:

- Use the Wasp TS Config conversion guide for the config conversion.
- After converting the config, return to the 0.23 to 0.24 migration guide and finish the shared migration steps.
- Keep the app's behavior the same.
- Prefer reference imports with \`with { type: "ref" }\` when importing components and functions from src.
- If splitting the spec into multiple files, export \`Spec\` from feature spec files and join them into \`main.wasp.ts\`.

Please make the changes directly in the repo and tell me what commands I should run to verify the migration.
`}
</LLMPrompt>
</TabItem>
</Tabs>

If you want to do it manually, follow the steps below.

### 1. Bump the Wasp version

Update the version field in your Wasp config to `^0.24.0`. The syntax depends on which config style your app currently uses.

<Tabs sideBySide>
  <TabItem value="wasp-dsl" label="Wasp DSL">
    ```wasp title="main.wasp"
    app MyApp {
      wasp: {
        version: "^0.24.0"
      },
      // ...
    }
    ```
  </TabItem>
  <TabItem value="wasp-ts-config" label="Wasp TS Config">
    ```ts title="main.wasp.ts"
    import { App } from "wasp-config"

    const app = new App("MyApp", {
      wasp: {
        version: "^0.24.0",
      },
      // ...
    })
    ```
  </TabItem>
</Tabs>

### 2. Convert your config to the Wasp Spec

- If your app has a `main.wasp` file, follow [Migrating from the Wasp DSL](./guides/legacy/wasp-dsl.md). This converts your app from the Wasp DSL to the Wasp Spec.
- If your app already has a `main.wasp.ts` file using the old class-based `new App(...)` API, follow [Migrating from the Wasp TS Config](./guides/legacy/wasp-ts-config.md). This converts your app from the old TS Config to the Wasp Spec.

After you finish the conversion guide, **come back here** and continue with the shared migration steps below.

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

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```ts
    import { api } from "wasp/client/api"

    // Making requests
    const response = await api.get("/foo/bar")
    const data = response.data

    // POST with body
    await api.post("/foo/bar", { key: "value" })

    // Error handling
    import { type AxiosError } from "axios"
    try {
      await api.get("/foo/bar")
    } catch (e) {
      const error = e as AxiosError
      console.log(error.response?.status)
    }
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```ts
    import { api } from "wasp/client/api"
    import { isHTTPError } from "ky"

    // Making requests
    const data = await api.get("/foo/bar").json()

    // POST with body
    await api.post("/foo/bar", { json: { key: "value" } })

    // Error handling
    try {
      await api.get("/foo/bar").json()
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
