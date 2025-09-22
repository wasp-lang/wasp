---
title: Wasp TypeScript config (*.wasp.ts)
---

import DiscordLink from '@site/blog/components/DiscordLink';

:::caution Requires Wasp >= 0.16.3
This document assumes your app works with Wasp >= 0.16.3.\
If you haven't migrated your app yet, follow the [migration instructions](../migration-guides/migrate-from-0-15-to-0-16.md) and verify everything works. After that, come back here and try out the new Wasp TS config.
:::

:::caution Early preview
This feature is currently in early preview and we are actively working on it.
:::

:::caution Running `wasp ts-setup`
Whenever you run run `wasp clean` or remove `node_modules` on  your own, you must rerun `wasp ts-setup`! We will remove this requirement in future versions. Read more about it below.
:::

In Wasp, you normally define/configure the high level of your app (pages, routes, queries, actions, auth, ...) in a `main.wasp` file in the root of your project. In `main.wasp` you write in Wasp's DSL (domain-specific language), which is a simple configuration language similar to JSON but smarter.

Wasp recently introduced the **Wasp TS config**, an alternative way to define the high level of your app via `main.wasp.ts`! Although it looks similar to how you would do it in `main.wasp`, the difference is that you write in TypeScript, not in Wasp's DSL.

Wasp TS config is an **early preview** feature, meaning it is a little rough and not yet where it could be, but it does work. We think it's pretty cool already, and you can try it out now. If you do, please share your feedback and ideas with us on our [GitHub](https://github.com/wasp-lang/wasp) or <DiscordLink />. This is crucial for us to be able to shape this feature in the best possible way!

## Motivation

- Out-of-the-box support in all editors.
- Less maintenance on our side.
- More flexibility for you while writing the config.
- It will enable us to easily add support for multiple Wasp files in the future.
- A great foundation for the Full Stack Modules (FSM) that are a part of our future plans.

## How to switch from the Wasp DSL config to the Wasp TS config

1. Go into the Wasp project you want to switch to the Wasp TS config (or create a new Wasp project if you want to try it out like that). Make sure you are on Wasp >= 0.16.3 and your project is working.
2. Rename `tsconfig.json` file to `tsconfig.src.json`
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

       "module": "NodeNext",
       "noEmit": true,

       "lib": ["ES2023"]
     },
     "include": ["main.wasp.ts"]
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

8. Run `wasp ts-setup`. This command will add the `wasp-config` package to your `package.json`'s `devDependencies`.

   **IMPORTANT:** Every time you run `wasp clean` or delete your `node_modules`, you _must_ follow it up with `wasp ts-setup`. This is a temporary meassure until we improve the feature.

9. Create an empty `main.wasp.ts` file and rewrite your `main.wasp.old` in it but in TypeScript.

   Check out the [reference main.wasp.ts file](#reference-mainwaspts-file) below for details on what the TypeScript API for configuring Wasp looks like.
   In short, you'll have to:

   1. Import `App` from `wasp-config`
   2. Create a new `app` object with `new App()`.
   3. Use the `app` object to define parts of your web app like `auth`, `pages`, `query`, `api`...
   4. Export the `app` from your file using a default export.

   You can manually do the rewrite using the reference file and TS types as guides (IDE support should work for you in `main.wasp.ts`), or you can (and we recommend it!) give the reference main.wasp.ts file to the LLM of your choice and tell it to rewrite your `main.wasp` while following the format in the reference file: we had great results with this!

10. Run `wasp start` to run your app! If you got everything right, your app should work exactly like it did before. The only difference is that it's now reading the Wasp config from `main.wasp.ts` instead of `main.wasp`.
    :::tip
    Don't forget, during `wasp start`, to have the database running or do the db migrations if needed, as you would normally when running your app in development.
    :::

11. That is it, you are now using Wasp TS config! You can delete `main.wasp.old` file now if you still have it around.

:::caution
If you run `wasp clean` or remove `node_modules` on  your own, you will have to rerun `wasp ts-setup`! This is a temporary workaround, we will remove it in future versions.
:::

Got stuck on any of these steps? Let us know in our <DiscordLink /> and we will help!

## What next?

### Experiment

Play with the Wasp TS config, get the feel of it, and see if you can find ways to improve it. Here are some ideas you can experiment with:

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
import { App } from 'wasp-config'

const app = new App('todoApp', {
  title: 'ToDo App',
  wasp: { version: '{latestWaspVersion}' },
  // head: []
});

app.webSocket({
  fn: { import: 'webSocketFn', from: '@src/webSocket' },
  // autoConnect: false
});

app.auth({
  userEntity: 'User',
  methods: {
    discord: {
      configFn: { import: 'config', from: '@src/auth/discord' },
      userSignupFields: { import: 'userSignupFields', from: '@src/auth/discord' }
    },
    google: {
      configFn: { import: 'config', from: '@src/auth/google' },
      userSignupFields: { import: 'userSignupFields', from: '@src/auth/google' }
    },
    gitHub: {
      configFn: { import: 'config', from: '@src/auth/github.js' },
      userSignupFields: { import: 'userSignupFields', from: '@src/auth/github.js' }
    },
    // keycloak: {},
    // email: {
    //   userSignupFields: { import: 'userSignupFields', from: '@src/auth/email' },
    //   fromField: {
    //     name: 'ToDO App',
    //     email: 'mihovil@ilakovac.com'
    //   },
    //   emailVerification: {
    //     getEmailContentFn: { import: 'getVerificationEmailContent', from: '@src/auth/email' },
    //     clientRoute: 'EmailVerificationRoute',
    //   },
    //   passwordReset: {
    //     getEmailContentFn: { import: 'getPasswordResetEmailContent', from: '@src/auth/email' },
    //     clientRoute: 'PasswordResetRoute'
    //   }
    // },
  },
  onAuthFailedRedirectTo: '/login',
  onAuthSucceededRedirectTo: '/profile',
  onBeforeSignup: { import: 'onBeforeSignup', from: '@src/auth/hooks.js' },
  onAfterSignup: { import: 'onAfterSignup', from: '@src/auth/hooks.js' },
  onAfterEmailVerified: { import: 'onAfterEmailVerified', from: "@src/auth/hooks.ts" },
  onBeforeOAuthRedirect: { import: 'onBeforeOAuthRedirect', from: '@src/auth/hooks.js' },
  onBeforeLogin: { import: 'onBeforeLogin', from: '@src/auth/hooks.js' },
  onAfterLogin: { import: 'onAfterLogin', from: '@src/auth/hooks.js' }
});

app.server({
  setupFn: { importDefault: 'setup', from: '@src/serverSetup' },
  middlewareConfigFn: { import: 'serverMiddlewareFn', from: '@src/serverSetup' },
});

app.client({
  rootComponent: { import: 'App', from: '@src/App' },
  setupFn: { importDefault: 'setup', from: '@src/clientSetup' }
});

app.db({
  seeds: [
    { import: 'devSeedSimple', from: '@src/dbSeeds' },
  ]
});

app.emailSender({
  provider: 'SMTP',
  defaultFrom: { email: 'test@test.com' }
});

const loginPage = app.page('LoginPage', {
  component: { importDefault: 'Login', from: '@src/pages/auth/Login' }
});
app.route('LoginRoute', { path: '/login', to: loginPage });

app.query('getTasks', {
  fn: { import: 'getTasks', from: '@src/queries' },
  entities: ['Task']
});

app.action('createTask', {
  fn: { import: 'createTask', from: '@src/actions' },
  entities: ['Task']
});

app.apiNamespace('bar', {
  middlewareConfigFn: { import: 'barNamespaceMiddlewareFn', from: '@src/apis' },
  path: '/bar'
});

app.api('barBaz', {
  fn: { import: 'barBaz', from: '@src/apis' },
  auth: false,
  entities: ['Task'],
  httpRoute: {
    method: 'GET',
    route: '/bar/baz',
  },
});

app.job('mySpecialJob', {
  executor: 'PgBoss',
  perform: {
    fn: { import: 'foo', from: '@src/jobs/bar' },
    executorOptions: {
      pgBoss: { retryLimit: 1 }
    }
  },
  entities: ['Task']
});

export default app;
```
