---
title: Migrating from the Wasp TS Config to the Wasp Spec
description: Wasp 0.24 retired the class-based TS Config (new App) for the function-based Wasp Spec. Convert the app, specifications, imports, APIs, jobs and CRUD.
sidebar_label: "Wasp TS Config"
comments: true
---

import LastCheckedWithVersionsNotice from "@site/src/components/LastCheckedWithVersionsNotice";

# Migrating from the Wasp TS Config

<LastCheckedWithVersionsNotice versions={{ Wasp: "0.27" }} />

The first version of configuring Wasp in TypeScript used a **class-based API**: you created an `App` instance with `new App(...)` and registered declarations with mutating method calls like `app.page(...)` and `app.query(...)`. We called this the **TS Config**.

Wasp 0.24 retired the TS Config in favor of the [Wasp Spec](../../features/spec.md), a **function-based API** where you call `app({ ... })` once and list everything in a `spec` property.

If your project's `main.wasp.ts` still calls `new App(...)`, it runs Wasp 0.23 or older. This guide converts it straight to the Wasp Spec setup of Wasp 0.27. [How to migrate](#how-to-migrate) also lists the migration guides for the other changes between your version and 0.27.

## New features

### Reference imports

In the TS Config you could only reference your code with import objects (`{ import, from }`). The Wasp Spec also supports **reference imports**: import the value with the regular `import` syntax and pass it directly to a specification constructor.

<Tabs sideBySide>
  <TabItem value="before" label="TS Config">
    ```ts title="main.wasp.ts"
    const mainPage = app.page("MainPage", {
      component: { importDefault: "MainPage", from: "@src/MainPage" },
    });

    app.query("getTasks", {
      fn: { import: "getTasks", from: "@src/queries" },
    });
    ```

  </TabItem>
  <TabItem value="after" label="Wasp Spec">
    ```ts title="main.wasp.ts"
    import MainPage from "./src/MainPage" with { type: "ref" };
    import { getTasks } from "./src/queries" with { type: "ref" };

    export default app({
      // ...
      spec: [
        route("MainRoute", "/", page(MainPage)),
        query(getTasks),
      ],
    });
    ```

  </TabItem>
</Tabs>

Import objects still work through the `ref(...)` helper, so you can migrate gradually. See the [Wasp Spec documentation](../../features/spec.md#referencing-your-apps-code) for the supported patterns and their limitations.

### Multiple files

The TS Config required your entire configuration to live in a single `main.wasp.ts`. The Wasp Spec lets you split it across multiple `*.wasp.ts` files and import specifications between them, so you can keep large apps organized (for example, a separate `auth.wasp.ts` or `cards.wasp.ts` next to the feature it configures).

See the [Wasp Spec documentation](../../features/spec.md#splitting-your-spec-into-multiple-files) for details.

## Changes

### Overview

| What                      | Before                                                                                                                                            | After                                                                                                                                   |
| ------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------- |
| Creating an app           | `new App(name, { ... });`                                                                                                                         | `app({ name, ..., spec: [...] });`                                                                                                      |
| Configuring the app       | `app.auth(...);` <br/> `app.server(...);` <br/> `app.client(...);` <br/> `app.db(...);` <br/> `app.emailSender(...);` <br/> `app.webSocket(...);` | <pre>app(\{<br/> auth: ...,<br/> server: ...,<br/> client: ...,<br/> db: ...,<br/> emailSender: ...,<br/> webSocket: ...,<br/>});</pre> |
| Adding app specifications | `app.route(...);` <br/> `app.query(...);` <br/> `app.action(...);` <br/> etc                                                                      | <pre>app(\{<br/> spec: [<br/> route(...),<br/> query(...),<br/> action(...),<br/> ]<br/>});</pre>                                       |
| Imports                   | `{ import, from }`                                                                                                                                | `import { ... } from "./src/..." with { type: "ref" };`                                                                                 |
| Package name              | `wasp-config`                                                                                                                                     | `@wasp/spec`                                                                                                                         |
| Wasp version              | `wasp: { version: "^0.23.0" }` in `new App(...)`                                                                                                  | `"@wasp/wasp": "0.27.0"` in `package.json`                                                                                           |

### App and specifications

<Tabs sideBySide>
  <TabItem value="before" label="TS Config">
    ```ts title="main.wasp.ts"
    import { App } from "wasp-config";

    const app = new App("todoApp", {
      title: "ToDo App",
      wasp: { version: "^0.23.0" },
    });

    const mainPage = app.page("MainPage", {
      component: { importDefault: "MainPage", from: "@src/MainPage" },
    });
    app.route("MainRoute", { path: "/", to: mainPage });

    app.query("getTasks", {
      fn: { import: "getTasks", from: "@src/queries" },
      entities: ["Task"],
    });

    export default app;
    ```

  </TabItem>
  <TabItem value="after" label="Wasp Spec">
    ```ts title="main.wasp.ts"
    import { app, page, query, route } from "@wasp/spec";
    import MainPage from "./src/MainPage" with { type: "ref" };
    import { getTasks } from "./src/queries" with { type: "ref" };

    export default app({
      name: "todoApp",
      title: "ToDo App",
      spec: [
        route("MainRoute", "/", page(MainPage)),
        query(getTasks, { entities: ["Task"] }),
      ],
    });
    ```

  </TabItem>
</Tabs>

The Wasp Spec has no `wasp` field. The `@wasp/wasp` version in `package.json` sets your project's Wasp version.

### API: `httpRoute` becomes positional arguments

<Tabs sideBySide>
  <TabItem value="before" label="TS Config">
    ```ts title="main.wasp.ts"
    app.apiNamespace("bar", {
      middlewareConfigFn: { import: "barNamespaceMiddlewareFn", from: "@src/apis" },
      path: "/bar",
    });

    app.api("barBaz", {
      fn: { import: "barBaz", from: "@src/apis" },
      auth: false,
      entities: ["Task"],
      httpRoute: { method: "GET", route: "/bar/baz" },
    });
    ```

  </TabItem>
  <TabItem value="after" label="Wasp Spec">
    ```ts title="main.wasp.ts"
    import { api, apiNamespace, app } from "@wasp/spec";
    import { barBaz, barNamespaceMiddlewareFn } from "./src/apis" with { type: "ref" };

    export default app({
      // ...
      spec: [
        apiNamespace("/bar", {
          middlewareConfigFn: barNamespaceMiddlewareFn,
        }),
        api("GET", "/bar/baz", barBaz, { auth: false, entities: ["Task"] }),
      ],
    });
    ```

  </TabItem>
</Tabs>

### Jobs: `perform` is flattened

<Tabs sideBySide>
  <TabItem value="before" label="TS Config">
    ```ts title="main.wasp.ts"
    app.job("mySpecialJob", {
      executor: "PgBoss",
      perform: {
        fn: { import: "foo", from: "@src/jobs/bar" },
        executorOptions: { pgBoss: { retryLimit: 1 } },
      },
      entities: ["Task"],
    });
    ```
  </TabItem>
  <TabItem value="after" label="Wasp Spec">
    ```ts title="main.wasp.ts"
    import { app, job } from "@wasp/spec";
    import { foo } from "./src/jobs/bar" with { type: "ref" };

    export default app({
      // ...
      spec: [
        job(foo, {
          executor: "PgBoss",
          entities: ["Task"],
          performExecutorOptions: { pgBoss: { retryLimit: 1 } },
        }),
      ],
    });
    ```

  </TabItem>
</Tabs>

### CRUD

<Tabs sideBySide>
  <TabItem value="before" label="TS Config">
    ```ts title="main.wasp.ts"
    app.crud("tasks", {
      entity: "Task",
      operations: {
        getAll: {},
        create: { overrideFn: { import: "createTask", from: "@src/actions" } },
      },
    });
    ```
  </TabItem>
  <TabItem value="after" label="Wasp Spec">
    ```ts title="main.wasp.ts"
    import { app, crud } from "@wasp/spec";
    import { createTask } from "./src/actions" with { type: "ref" };

    export default app({
      // ...
      spec: [
        crud("tasks", "Task", {
          getAll: {},
          create: { overrideFn: createTask },
        }),
      ],
    });
    ```

  </TabItem>
</Tabs>

### Top-level config: `auth`, `server`, `client`, `db`, `emailSender`, `webSocket`

These were configured with mutating method calls. They are now keys of the `app({ ... })` object.

<Tabs sideBySide>
  <TabItem value="before" label="TS Config">
    ```ts title="main.wasp.ts"
    const app = new App("todoApp", {
      title: "ToDo App",
    });

    app.auth({
      userEntity: "User",
      methods: { google: {} },
      onAuthFailedRedirectTo: "/login",
    });

    app.client({
      rootComponent: { importDefault: "App", from: "@src/App" },
    });

    app.emailSender({
      provider: "SMTP",
      defaultFrom: { email: "hi@example.com" },
    });

    export default app;
    ```

  </TabItem>
  <TabItem value="after" label="Wasp Spec">
    ```ts title="main.wasp.ts"
    import { app } from "@wasp/spec";
    import App from "./src/App" with { type: "ref" };

    export default app({
      name: "todoApp",
      title: "ToDo App",
      auth: {
        userEntity: "User",
        methods: { google: {} },
        onAuthFailedRedirectTo: "/login",
      },
      client: {
        rootComponent: App,
      },
      emailSender: {
        provider: "SMTP",
        defaultFrom: { email: "hi@example.com" },
      },
      // ...
    });
    ```

  </TabItem>
</Tabs>

## How to migrate

These steps convert a class-based Wasp TS Config project to the Wasp Spec setup of Wasp 0.27. The Wasp versions between yours and 0.27 changed other parts of a project too, so step 6 sends you through their migration guides.

Wasp validates the Wasp Spec support files during migration, including the required `package.json` entries, `tsconfig.wasp.json` options, and `tsconfig.src.json` exclusions.

1.  Replace `wasp-config` with Wasp in your `package.json`:

    <Tabs sideBySide>
      <TabItem value="before" label="Before">
        ```json title="package.json"
        {
          // ...
          "devDependencies": {
            // ...
            "wasp-config": "file:.wasp/wasp-config"
          }
        }
        ```
      </TabItem>
      <TabItem value="after" label="After">
        ```json title="package.json"
        {
          // ...
          "scripts": {
            "wasp": "wasp"
          },
          "dependencies": {
            // ...
            "@wasp/wasp": "0.27.0"
          },
          "devDependencies": {
            // ...
            "@types/node": "^24.0.0"
          }
        }
        ```
      </TabItem>
    </Tabs>

    Keep your existing dependencies, replace `wasp-config` with `@wasp/wasp`, and add `@types/node`. `@wasp/wasp` sets your project's Wasp version and brings the Wasp CLI and the Wasp Spec. The Wasp Spec runs in Node.js, so it needs `@types/node`. The `wasp` script is optional. It runs your project's Wasp through npm, for example `npm run wasp -- db migrate-dev`. Read more in [Running Wasp commands](../../advanced/cli.md#running-wasp-commands).

2.  If your `.npmrc` sets `min-release-age`, exclude Wasp's packages from it. Otherwise npm won't install a new Wasp release during its first 7 days:

    ```ini title=".npmrc"
    min-release-age=7
    # highlight-next-line
    min-release-age-exclude[]=@wasp/*
    ```

3.  Replace the content of your `tsconfig.wasp.json` with the following:

    ```json title="tsconfig.wasp.json"
    {
      "compilerOptions": {
        "skipLibCheck": true,
        "target": "ES2025",
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
        "lib": ["ES2025"],
        "types": ["node"]
      },
      "include": ["**/*.wasp.ts", ".wasp/out/types/spec"]
    }
    ```

4.  Keep the compiler options in your `tsconfig.src.json` and add the entries Wasp needs:

    ```json title="tsconfig.src.json"
    {
      "compilerOptions": {
        // ...
        // highlight-next-line
        "types": ["react", "node"]
      },
      // highlight-start
      "include": ["src", ".wasp/out/types/app"],
      "exclude": ["**/*.wasp.ts"]
      // highlight-end
    }
    ```

5.  Rewrite `main.wasp.ts`:

    Replace `new App(...)` and the `app.*(...)` method calls with a single `app({ ... })` call whose `spec` property holds the specifications. The [mapping above](#changes) shows how each call converts. Update the import, and leave out the `wasp: { version }` field, since step 1 set the Wasp version in `package.json`:

    <Tabs sideBySide>
      <TabItem value="before" label="Before">
        ```ts title="main.wasp.ts"
        import { App } from "wasp-config";

        const app = new App("myApp", {
          title: "My app",
          wasp: { version: "^0.23.0" },
        });
        ```

      </TabItem>
      <TabItem value="after" label="After">
        ```ts title="main.wasp.ts"
        import { app } from "@wasp/spec";

        export default app({
          name: "myApp",
          title: "My app",
          head: ["<link rel='icon' href='/favicon.ico' />"],
          spec: [
            // ...
          ]
        });
        ```

      </TabItem>
    </Tabs>

    :::note
    While previously we accepted any `*.wasp.ts` file name, with the Wasp Spec the entry file must be named `main.wasp.ts`. You can still split the rest of your config across other `*.wasp.ts` files.
    :::

6.  Make the other changes from the migration guides between your Wasp version and 0.27. For a project on Wasp 0.23, follow these in order:

    - [Migration from 0.23 to 0.24](/docs/0.24/migration-guide)
    - [Migration from 0.24 to 0.25](/docs/0.25/migration-guide)
    - [Migration from 0.25 to 0.26](/docs/0.26/migration-guide)
    - [Migration from 0.26 to 0.27](../../migration-guide.md)

    If your project runs an older Wasp version, start with the migration guide from your version, for example [Migration from 0.22 to 0.23](/docs/0.23/migration-guide).

    The steps above already cover parts of these guides. Skip the migration guide steps that:

    - bump or remove the Wasp version, or run `wasp install`
    - convert the config to the Wasp Spec, or rename the Wasp Spec package
    - change `tsconfig.json`, `tsconfig.src.json`, or `tsconfig.wasp.json`
    - add `@wasp/wasp` to `package.json`, or exclude it in `.npmrc`

7.  Install the dependencies, run the database migrations, and start your app:

    ```sh
    wasp clean
    npm install
    wasp db migrate-dev
    wasp start
    ```

    `@wasp/wasp` needs the dependency versions from the migration guides, like Prisma 6 and TypeScript 6. Until your `package.json` lists them, `npm install` fails with a dependency conflict. If everything is correct, your app behaves exactly as before.

See the full [Wasp Spec reference](../../features/spec.md#reference) for every option. Got stuck? Reach out on our [Discord](https://discord.gg/rzdnErX) and we'll help.
