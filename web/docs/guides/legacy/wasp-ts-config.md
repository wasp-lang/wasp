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

If your project's `main.wasp.ts` still calls `new App(...)`, it runs Wasp 0.23 or older. This guide shows how each part of the TS Config looks in the Wasp Spec, and [How to migrate](#how-to-migrate) shows how to upgrade your project.

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
    import { app, page, query, route } from "@wasp/spec";
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

Upgrade your project one Wasp version at a time with the migration guides, starting with the one from your project's version, for example [Migration from 0.22 to 0.23](/docs/0.23/migration-guide).

When you reach [Migration from 0.23 to 0.24](/docs/0.24/migration-guide), convert your config with [the Wasp 0.24 version of this guide](/docs/0.24/guides/legacy/wasp-ts-config#how-to-migrate), which has the steps for Wasp 0.24. Then continue with the migration guides up to the [latest one](../../migration-guide.md).

The [changes above](#changes) show the Wasp Spec of the latest Wasp version, so they show how your config looks once you finish.

See the full [Wasp Spec reference](../../features/spec.md#reference) for every option. Got stuck? Reach out on our [Discord](https://discord.gg/rzdnErX) and we'll help.
