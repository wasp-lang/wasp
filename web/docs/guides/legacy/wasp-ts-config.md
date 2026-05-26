---
sidebar_label: "Wasp TS Config"
comments: true
last_checked_with_versions:
  Wasp: 0.24
---

# Migrating from the Wasp TS Config

The first version of configuring Wasp in TypeScript used a **class-based API**: you created an `App` instance with `new App(...)` and registered parts with mutating method calls like `app.page(...)` and `app.query(...)`. We called this the **TS Config**.

Starting with Wasp 0.24, the TS Config is now retired in favor of the [Wasp Spec](../../general/spec.md): a **function-based API** where you call `app({ ... })` once and list everything in a `parts` array.

:::tip Let an LLM do the heavy lifting
The mapping below is mechanical. You can give the [Wasp Spec reference](../../general/spec.md#reference) and your old `main.wasp.ts` to the LLM of your choice and ask it to rewrite it following that reference.
:::

## New features

### Reference imports

In the TS Config you could only reference your code with import objects (`{ import, from }`). The Wasp Spec also supports **reference imports**: import the value with the regular `import` syntax and pass it directly to a constructor.

<Tabs>
  <TabItem value="before" label="TS Config">
    ```ts title="main.wasp.ts"
    const mainPage = app.page('MainPage', {
      component: { importDefault: 'MainPage', from: '@src/MainPage' },
    })

    app.query('getTasks', {
      fn: { import: 'getTasks', from: '@src/queries' },
    })
    ```
  </TabItem>
  <TabItem value="after" label="Wasp Spec">
    ```ts title="main.wasp.ts"
    import MainPage from './src/MainPage' with { type: "ref" }
    import { getTasks } from './src/queries' with { type: "ref" }

    export default app({
      // ...
      parts: [
        route('MainRoute', '/', page(MainPage)),
        query(getTasks),
      ],
    })
    ```
  </TabItem>
</Tabs>

Import objects still work, so you can migrate gradually. See the [Wasp Spec documentation](../../general/spec.md#referencing-your-apps-code) for the supported patterns and their limitations.

### Multiple files

The TS Config required your entire configuration to live in a single `main.wasp.ts`. The Wasp Spec lets you split it across multiple `*.wasp.ts` files and import parts between them, so you can keep large configs organized (for example, a separate `auth.wasp.ts` or `cards.wasp.ts` next to the feature it configures).

See the [Wasp Spec documentation](../../general/spec.md#splitting-your-spec-into-multiple-files) for details.

## Changes

### Overview

| What | Before | After |
| --- | --- | --- |
| Creating an app | `new App(name, { ... })` | `app({ name, ..., parts: [] })` |
| Configuring the app | `app.auth(...)` <br/> `app.server(...)` <br/> `app.client(...)` <br/> `app.db(...)` <br/> `app.emailSender(...)` <br/> `app.webSocket(...)` | <pre>app(\{<br/>  auth: ...,<br/>  server: ...,<br/>  client: ...,<br/>  db: ...,<br/>  emailSender: ...,<br/>  webSocket: ...,<br/>})</pre> |
| Adding app declarations | `app.route(...)` <br/> `app.query(...)` <br/> `app.action(...)` <br/> etc | <pre>app(\{<br/>  parts: [<br/>    route(...),<br/>    query(...),<br/>    action(...),<br/>  ]<br/>})</pre> |
| Imports | `{ import, from }` | `import { ... } from "./src/..." with { type: "ref" }` |
| Package name | `wasp-config` | `@wasp.sh/spec` |

### App and parts

<Tabs>
  <TabItem value="before" label="TS Config">
    ```ts title="main.wasp.ts"
    import { App } from 'wasp-config'

    const app = new App('todoApp', {
      title: 'ToDo App',
      wasp: { version: '^0.24.0' },
    })

    const mainPage = app.page('MainPage', {
      component: { importDefault: 'MainPage', from: '@src/MainPage' },
    })
    app.route('MainRoute', { path: '/', to: mainPage })

    app.query('getTasks', {
      fn: { import: 'getTasks', from: '@src/queries' },
      entities: ['Task'],
    })

    export default app
    ```
  </TabItem>
  <TabItem value="after" label="Wasp Spec">
    ```ts title="main.wasp.ts"
    import { app, page, query, route } from '@wasp.sh/spec'
    import MainPage from './src/MainPage' with { type: "ref" }
    import { getTasks } from './src/queries' with { type: "ref" }

    export default app({
      name: 'todoApp',
      title: 'ToDo App',
      wasp: { version: '^0.24.0' },
      parts: [
        route('MainRoute', '/', page(MainPage)),
        query(getTasks, { entities: ['Task'] }),
      ],
    })
    ```
  </TabItem>
</Tabs>

### API: `httpRoute` becomes positional arguments

<Tabs>
  <TabItem value="before" label="TS Config">
    ```ts title="main.wasp.ts"
    app.apiNamespace('bar', {
      middlewareConfigFn: { import: 'barNamespaceMiddlewareFn', from: '@src/apis' },
      path: '/bar',
    })

    app.api('barBaz', {
      fn: { import: 'barBaz', from: '@src/apis' },
      auth: false,
      entities: ['Task'],
      httpRoute: { method: 'GET', route: '/bar/baz' },
    })
    ```
  </TabItem>
  <TabItem value="after" label="Wasp Spec">
    ```ts title="main.wasp.ts"
    import { api, apiNamespace, app } from '@wasp.sh/spec'
    import { barBaz, barNamespaceMiddlewareFn } from './src/apis' with { type: "ref" }

    export default app({
      // ...
      parts: [
        apiNamespace('/bar', {
          middlewareConfigFn: barNamespaceMiddlewareFn,
        }),
        api('GET', '/bar/baz', barBaz, { auth: false, entities: ['Task'] }),
      ],
    })
    ```
  </TabItem>
</Tabs>

### Jobs: `perform` is flattened

<Tabs>
  <TabItem value="before" label="TS Config">
    ```ts title="main.wasp.ts"
    app.job('mySpecialJob', {
      executor: 'PgBoss',
      perform: {
        fn: { import: 'foo', from: '@src/jobs/bar' },
        executorOptions: { pgBoss: { retryLimit: 1 } },
      },
      entities: ['Task'],
    })
    ```
  </TabItem>
  <TabItem value="after" label="Wasp Spec">
    ```ts title="main.wasp.ts"
    import { app, job } from '@wasp.sh/spec'
    import { foo } from './src/jobs/bar' with { type: "ref" }

    export default app({
      // ...
      parts: [
        job(foo, {
          executor: 'PgBoss',
          entities: ['Task'],
          performExecutorOptions: { pgBoss: { retryLimit: 1 } },
        }),
      ],
    })
    ```
  </TabItem>
</Tabs>

### CRUD

<Tabs>
  <TabItem value="before" label="TS Config">
    ```ts title="main.wasp.ts"
    app.crud('tasks', {
      entity: 'Task',
      operations: {
        getAll: {},
        create: { overrideFn: { import: 'createTask', from: '@src/actions' } },
      },
    })
    ```
  </TabItem>
  <TabItem value="after" label="Wasp Spec">
    ```ts title="main.wasp.ts"
    import { app, crud } from '@wasp.sh/spec'
    import { createTask } from './src/actions' with { type: "ref" }

    export default app({
      // ...
      parts: [
        crud('tasks', 'Task', {
          getAll: {},
          create: { overrideFn: createTask },
        }),
      ],
    })
    ```
  </TabItem>
</Tabs>

### Top-level config: `auth`, `server`, `client`, `db`, `emailSender`, `webSocket`

These were configured with mutating method calls. They are now keys of the `app({ ... })` object.

<Tabs>
  <TabItem value="before" label="TS Config">
    ```ts title="main.wasp.ts"
    const app = new App('todoApp', {
      title: 'ToDo App',
      wasp: { version: '^0.24.0' },
    })

    app.auth({
      userEntity: 'User',
      methods: { google: {} },
      onAuthFailedRedirectTo: '/login',
    })

    app.client({
      rootComponent: { importDefault: 'App', from: '@src/App' },
    })

    app.emailSender({
      provider: 'SMTP',
      defaultFrom: { email: 'hi@example.com' },
    })

    export default app
    ```
  </TabItem>
  <TabItem value="after" label="Wasp Spec">
    ```ts title="main.wasp.ts"
    import { app } from '@wasp.sh/spec'
    import App from './src/App' with { type: "ref" }

    export default app({
      name: 'todoApp',
      title: 'ToDo App',
      wasp: { version: '^0.24.0' },
      auth: {
        userEntity: 'User',
        methods: { google: {} },
        onAuthFailedRedirectTo: '/login',
      },
      client: {
        rootComponent: App,
      },
      emailSender: {
        provider: 'SMTP',
        defaultFrom: { email: 'hi@example.com' },
      },
      parts: [],
    })
    ```
  </TabItem>
</Tabs>

## How to migrate

These steps assume your project is already on Wasp `^0.24.0`. If it isn't, follow the [migration guide](../../migration-guide.md) first.

Wasp validates the Wasp Spec support files during migration, including the required `package.json` entries, `tsconfig.wasp.json` options, and `tsconfig.src.json` exclusions.

1. Update your `package.json` with the new dependencies:

    <Tabs>
      <TabItem value="before" label="Before">
        ```json title="package.json"
        {
          "devDependencies": {
            "wasp-config": "file:.wasp/wasp-config"
          }
        }
        ```
      </TabItem>
      <TabItem value="after" label="After">
        ```json title="package.json"
        {
          "devDependencies": {
            "@types/node": "^24.0.0",
            "@wasp.sh/spec": "file:.wasp/spec"
          }
        }
        ```
      </TabItem>
    </Tabs>

    Keep your existing dependencies, replace `wasp-config` with `@wasp.sh/spec`, and add `@types/node`. `@types/node` is required because the Wasp Spec runs in a Node.js environment.

2. Update your `tsconfig.wasp.json` and make sure it includes the following settings:

    ```json title="tsconfig.wasp.json"
    {
      "compilerOptions": {
        "target": "ES2022",
        "module": "esnext",
        "moduleResolution": "bundler",
        "jsx": "preserve",
        "strict": true,
        "isolatedModules": true,
        "moduleDetection": "force",
        "skipLibCheck": true,
        "allowJs": true,
        "noEmit": true,
        "lib": ["ES2023"]
      },
      "include": ["main.wasp.ts", "**/*.wasp.ts"]
    }
    ```

3. Make sure your `tsconfig.src.json` excludes Wasp Spec files:

    ```json title="tsconfig.src.json"
    {
      // ...
      "include": ["src"],
      "exclude": ["**/*.wasp.ts"]
    }
    ```

4. Run `wasp install`.

5. Rewrite `main.wasp.ts`:

    Replace `new App(...)` and the `app.*(...)` method calls with a single `app({ ... })` call whose `parts` array holds the constructors (see the [mapping above](#changes)), and update the import:

    <Tabs>
      <TabItem value="before" label="Before">
        ```ts title="main.wasp.ts"
        import { App } from 'wasp-config'

        const app = new App('myApp', {
          title: 'My app',
          wasp: { version: '^0.24.0' },
        })
        ```
      </TabItem>
      <TabItem value="after" label="After">
        ```ts title="main.wasp.ts"
        import { app, page, route, query, action } from '@wasp.sh/spec'

        export default app({
          name: "myApp",
          title: 'My app',
          wasp: { version: '^0.24.0' },
          parts: [],
        })
        ```
      </TabItem>
    </Tabs>

6. Run your app with `wasp start`. If everything is correct, your app should behave exactly as before.

  :::note
  At some points, when the Spec needs to be regenerated, Wasp will tell you to run `wasp install` before being able to start the app. Usually, this might happen when upgrading Wasp versions, running `wasp clean`, or removing the `node_modules` folder.
  :::

See the full [Wasp Spec reference](../../general/spec.md#reference) for every option. Got stuck? Reach out on our [Discord](https://discord.gg/rzdnErX) and we'll help.
