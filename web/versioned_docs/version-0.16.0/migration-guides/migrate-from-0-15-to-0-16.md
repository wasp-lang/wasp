---
title: Migration from 0.15.X to 0.16.X
---

## What's new in 0.16.0?

### Env variables validation with Zod

Wasp now uses Zod to validate environment variables, allowing it to fail faster if something is misconfigured. This means you’ll get more relevant error messages when running your app with incorrect env variables.

You can also use Zod to validate your own environment variables. Here’s an example:

```ts
// src/env.ts
import * as z from 'zod'

import { defineEnvValidationSchema } from 'wasp/env'

export const serverEnvValidationSchema = defineEnvValidationSchema(
  z.object({
    STRIPE_API_KEY: z.string({
      required_error: 'STRIPE_API_KEY is required.',
    }),
  })
)

// main.wasp
app myApp {
  ...
  server: {
    envValidationSchema: import { serverEnvValidationSchema } from "@src/env",
  },
}
```

Read more about it in the [env variables](../project/env-vars.md#custom-env-var-validations) section of the docs.

## How to migrate?

To migrate your Wasp app from 0.15.X to 0.16.X, follow these steps:

### 1. Bump the Wasp version

Update the version field in your Wasp file to `^0.16.0`:

```wasp title="main.wasp"
app MyApp {
  wasp: {
    // highlight-next-line
    version: "^0.16.0"
  },
}
```

#### 1.1 Additional step for Wasp TS Config users

If you're using [Wasp's new TS config](../general/wasp-ts-config.md), you must
also rerun the `wasp ts-setup` command in your project. This command updates
the path for the `wasp-config` package in your `package.json`.

### 2. Update the `package.json` file

Make sure to explicitly add `react-dom` and `react-router-dom` to your `package.json` file:

```json
{
  "dependencies": {
    // highlight-start
    "react-dom": "^18.2.0",
    "react-router-dom": "^6.26.2"
    // highlight-end
  }
}
```

### 3. Update the `tsconfig.json` file

Wasp now internally works with TypeScript project references, so you'll have to
update your `tsconfig.json` (Wasp will validate your `tsconfig.json` and warn
you if you forget something). Here are all the properties you must change:

```json
{
  "compilerOptions": {
    // ...
    "composite": true,
    "skipLibCheck": true,
    "outDir": ".wasp/out/user"
  },
  "include": ["src"]
}
```

### 4. Enjoy your updated Wasp app

That's it!

You should now be able to run your app with the new Wasp 0.16.0.
