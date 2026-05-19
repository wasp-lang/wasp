---
title: Wasp TypeScript spec (*.wasp.ts)
---

import { DiscordLink } from '@site/blog/components/DiscordLink';

:::caution Early preview
This feature is currently in early preview and we are actively working on it.
:::

:::caution Running `wasp ts-setup`
Whenever you run run `wasp clean` or remove `node_modules` on  your own, you must rerun `wasp ts-setup`! We will remove this requirement in future versions. Read more about it below.
:::

In Wasp, you normally define/configure the high level of your app (pages, routes, queries, actions, auth, ...) in a `main.wasp` file in the root of your project. In `main.wasp` you write in Wasp's DSL (domain-specific language), which is a simple configuration language similar to JSON but smarter.

Wasp recently introduced the **Wasp TS spec**, an alternative way to define the high level of your app via `main.wasp.ts`! Although it looks similar to how you would do it in `main.wasp`, the difference is that you write in TypeScript, not in Wasp's DSL.

Wasp TS spec is an **early preview** feature, meaning it is a little rough and not yet where it could be, but it does work. We think it's pretty cool already, and you can try it out now. If you do, please share your feedback and ideas with us on our [GitHub](https://github.com/wasp-lang/wasp) or <DiscordLink />. This is crucial for us to be able to shape this feature in the best possible way!

## Motivation

- Out-of-the-box support in all editors.
- Less maintenance on our side.
- More flexibility for you while writing the config.
- It will enable us to easily add support for multiple Wasp files in the future.
- A great foundation for the Full Stack Modules (FSM) that are a part of our future plans.

## How to switch from the Wasp DSL config to the Wasp TS spec

1. Go into the Wasp project you want to switch to the Wasp TS spec (or create a new Wasp project if you just want to try it out).
2. Rename `tsconfig.json` file to `tsconfig.src.json` and make sure it excludes Wasp TS spec files:

   ```json title="tsconfig.src.json"
   {
     // ...
     "include": ["src"],
     "exclude": ["**/*.wasp.ts"]
   }
   ```
3. Create a new `tsconfig.json` file with the following content:

   ```json title="tsconfig.json"
   {
     "files": [],
     "references": [
       { "path": "./tsconfig.src.json" },
       { "path": "./tsconfig.wasp.json" }
     ]
   }
   ```

4. Create a new `tsconfig.wasp.json` file with the following content:

   ```json title="tsconfig.wasp.json"
   {
     "compilerOptions": {
       "skipLibCheck": true,
       "target": "ES2022",
       "isolatedModules": true,
       "moduleDetection": "force",

       "strict": true,
       "noUnusedLocals": true,
       "noUnusedParameters": true,

        "module": "esnext",
        "moduleResolution": "bundler",
        "jsx": "preserve",
        "allowJs": true,
        "noEmit": true,

        "lib": ["ES2023"],
        "paths": {
          "@src/*": ["./src/*"]
        }
      },
      "include": ["main.wasp.ts", "**/*.wasp.ts"]
    }
    ```

5. Add `"type": "module"` to the top level of your `package.json`, if you don't have it yet:

   ```json title="package.json"
   {
     "type": "module",
     ...
   }
   ```

6. Rename the `main.wasp` file to `main.wasp.old`. You'll want to use it as a reference while writing `main.wasp.ts`.

7. Run `wasp clean` and `rm package-lock.json`. This ensures you start from a clean state.

8. Run `wasp ts-setup`. This command will add the `@wasp.sh/spec` package to your `package.json`'s `devDependencies`.

   **IMPORTANT:** Every time you run `wasp clean` or delete your `node_modules`, you _must_ follow it up with `wasp ts-setup`. This is a temporary meassure until we improve the feature.

9. Create an empty `main.wasp.ts` file and rewrite your `main.wasp.old` in it but in TypeScript.

   Check out the [reference main.wasp.ts file](#reference-mainwaspts-file) below for details on what the TypeScript API for configuring Wasp looks like.
   In short, you'll have to:

   1. Import Wasp constructors from `@wasp.sh/spec`.
   2. Create your app with `app({ ... })`.
   3. Define parts of your web app with constructors like `page`, `route`, `query`, `action`, and `api`.
   4. Export the result from your file using a default export.

   You can manually do the rewrite using the reference file and TS types as guides (IDE support should work for you in `main.wasp.ts`), or you can (and we recommend it!) give the reference main.wasp.ts file to the LLM of your choice and tell it to rewrite your `main.wasp` while following the format in the reference file: we had great results with this!

10. Run `wasp start` to run your app! If you got everything right, your app should work exactly like it did before. The only difference is that it's now reading the Wasp config from `main.wasp.ts` instead of `main.wasp`.
    :::tip
    Don't forget, during `wasp start`, to have the database running or do the db migrations if needed, as you would normally when running your app in development.
    :::

11. That is it, you are now using Wasp TS spec! You can delete `main.wasp.old` file now if you still have it around.

:::caution
If you run `wasp clean` or remove `node_modules` on  your own, you will have to rerun `wasp ts-setup`! This is a temporary workaround, we will remove it in future versions.
:::

Got stuck on any of these steps? Let us know in our <DiscordLink /> and we will help!

## Detecting the current environment

Wasp sets the `NODE_ENV` environment variable based on which command you use to run Wasp:
- `"development"` during `wasp start` (and any other command which may compile the project like `wasp db migrate-dev`).
- `"production"` during `wasp build`.

You can read this variable to switch config values per environment:

```ts title="main.wasp.ts"
const isProd = process.env.NODE_ENV === 'production'

export default app({
  name: 'todoApp',
  title: 'ToDo App',
  wasp: { version: '{latestWaspVersion}' },
  emailSender: {
    provider: isProd ? 'SMTP' : 'Dummy',
    defaultFrom: { email: 'hi@example.com' },
  },
  parts: [],
})
```

## What next?

### Experiment

Play with the Wasp TS spec, get the feel of it, and see if you can find ways to improve it. Here are some ideas you can experiment with:

- How would you reduce the boilerplate in `main.wasp.ts` file? Helper functions, loops?
- Can you imagine a better API or better abstractions? If you can, what would that look like? Perhaps you can even implement it on top of our API?
- Give a try at implementing your own file-based routing if that is what you like: you are now in Turing complete language and have access to the disk!
- Surprise us!

### Feedback

Whatever you end up doing, we would love it if you would let us know how it was and show us what you did.

We do have some immediate ideas of our own about what we want to improve, but we want to hear what you thought of, what you liked or disliked, or what you came up with. Even if you just found it all good, or just a single thing you didn't or did like, that is also valuable feedback and we would love to hear it!

Let us know on our [GitHub](https://github.com/wasp-lang/wasp) or, even better, in our <DiscordLink />.

## Reference main.wasp.ts file

```ts title="main.wasp.ts"
import {
  action,
  api,
  apiNamespace,
  app,
  job,
  page,
  query,
  route,
} from '@wasp.sh/spec'

import App from '@src/App'
import Login from '@src/pages/auth/Login'
import MainPage from '@src/pages/MainPage'
import setupClient from '@src/clientSetup'
import setupServer, { serverMiddlewareFn } from '@src/serverSetup'
import { config as discordConfig, userSignupFields as discordSignupFields } from '@src/auth/discord'
import { config as googleConfig, userSignupFields as googleSignupFields } from '@src/auth/google'
import {
  onAfterEmailVerified,
  onAfterLogin,
  onAfterSignup,
  onBeforeLogin,
  onBeforeOAuthRedirect,
  onBeforeSignup,
} from '@src/auth/hooks'
import { barBaz, barNamespaceMiddlewareFn } from '@src/apis'
import { createTask, getTasks } from '@src/operations'
import { devSeedSimple } from '@src/dbSeeds'
import { foo } from '@src/jobs/bar'
import { webSocketFn } from '@src/webSocket'

export default app({
  name: 'todoApp',
  title: 'ToDo App',
  wasp: { version: '{latestWaspVersion}' },
  head: [],
  webSocket: {
    fn: webSocketFn,
    autoConnect: false,
  },
  auth: {
    userEntity: 'User',
    methods: {
      discord: {
        configFn: discordConfig,
        userSignupFields: discordSignupFields,
      },
      google: {
        configFn: googleConfig,
        userSignupFields: googleSignupFields,
      },
    },
    onAuthFailedRedirectTo: '/login',
    onAuthSucceededRedirectTo: '/profile',
    onBeforeSignup,
    onAfterSignup,
    onAfterEmailVerified,
    onBeforeOAuthRedirect,
    onBeforeLogin,
    onAfterLogin,
  },
  server: {
    setupFn: setupServer,
    middlewareConfigFn: serverMiddlewareFn,
  },
  client: {
    rootComponent: App,
    setupFn: setupClient,
  },
  db: {
    seeds: [devSeedSimple],
  },
  emailSender: {
    provider: 'SMTP',
    defaultFrom: { email: 'test@test.com' },
  },
  parts: [
    route('LoginRoute', '/login', page(Login)),
    route('MainRoute', '/', page(MainPage, { authRequired: true })),
    query(getTasks, { entities: ['Task'] }),
    action(createTask, { entities: ['Task'] }),
    apiNamespace('/bar', { middlewareConfigFn: barNamespaceMiddlewareFn }),
    api('GET', '/bar/baz', barBaz, {
      auth: false,
      entities: ['Task'],
    }),
    job(foo, {
      executor: 'PgBoss',
      performExecutorOptions: {
        pgBoss: { retryLimit: 1 },
      },
      entities: ['Task'],
    }),
  ],
})
```
