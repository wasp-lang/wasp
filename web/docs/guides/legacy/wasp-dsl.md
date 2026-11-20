---
title: Migrating from the Wasp DSL to the Wasp Spec
sidebar_label: "Wasp DSL"
comments: true
---

import LastCheckedWithVersionsNotice from "@site/src/components/LastCheckedWithVersionsNotice";

# Migrating from the Wasp DSL

<LastCheckedWithVersionsNotice versions={{ Wasp: "0.27" }} />

Wasp used to have its own configuration language, the **Wasp DSL**, which you wrote in a `main.wasp` file. Wasp 0.24 retired the Wasp DSL in favor of the [Wasp Spec](../../features/spec.md), a `main.wasp.ts` file written in TypeScript.

If your project still has a `main.wasp` file, it runs Wasp 0.23 or older. This guide shows how each part of the Wasp DSL looks in the Wasp Spec, and [How to migrate](#how-to-migrate) shows how to upgrade your project.

## New features

### Just TypeScript

The Wasp DSL was a custom language, so it needed its own IDE extension for highlighting and autocompletion, and you couldn't use the JavaScript ecosystem inside it. The Wasp Spec is just TypeScript, so:

- No special IDE extension is needed. You get type checking, autocompletion, and go-to-definition from your editor's regular TypeScript support.
- You can `import` and use npm packages, environment variables, and your own helpers while building the config.
- You can use normal language features (variables, functions, loops, conditionals) to remove repetition from your config.

### Multiple files

The Wasp DSL kept your entire configuration in a single `main.wasp`. The Wasp Spec lets you split it across multiple `*.wasp.ts` files and import specifications between them, so you can keep large apps organized (for example, a separate `auth.wasp.ts` or `payments.wasp.ts` next to the feature it configures).

See the [Wasp Spec documentation](../../features/spec.md#splitting-your-spec-into-multiple-files) for details.

## Changes

### Overview

| What                      | Before                                                                   | After                                                                                            |
| ------------------------- | ------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------ |
| File name                 | `main.wasp`                                                              | `main.wasp.ts`                                                                                   |
| Creating an app           | `app Name { ... }`                                                       | `app({ name, ..., spec: [...] });`                                                                |
| Configuring the app       | <pre>app Name \{<br/> auth: \{ ... },<br/> server: \{ ... },<br/>}</pre> | <pre>app(\{<br/> auth: ...,<br/> server: ...,<br/>});</pre>                                       |
| Adding app specifications | <pre>route X \{ ... }<br/>query X \{ ... }<br/>action X \{ ... }</pre>   | <pre>app(\{<br/> spec: [<br/> route(...),<br/> query(...),<br/> action(...),<br/> ]<br/>});</pre> |
| Referencing code          | `import { x } from "@src/..."` inside a declaration                      | `import { ... } from "./src/..." with { type: "ref" };` at the top level                          |
| Entity references         | `Task` (identifier)                                                      | `"Task"` (string)                                                                                |
| Wasp version              | `wasp: { version: "^0.23.0" }` in `app`                                  | `"@wasp/wasp": "0.27.0"` in `package.json`                                                       |

### App, routes, and pages

In the DSL, a `route` points to a `page` by name. In the Wasp Spec, `route` takes the `page` object directly.

<Tabs sideBySide>
  <TabItem value="before" label="Wasp DSL">
    ```wasp title="main.wasp"
    app todoApp {
      title: "ToDo App",
      wasp: { version: "^0.23.0" }
    }

    route MainRoute { path: "/", to: MainPage }
    page MainPage {
      component: import { MainPage } from "@src/MainPage",
      authRequired: true
    }
    ```

  </TabItem>
  <TabItem value="after" label="Wasp Spec">
    ```ts title="main.wasp.ts"
    import { app, page, route } from "@wasp/spec";
    import { MainPage } from "./src/MainPage" with { type: "ref" };

    export default app({
      name: "todoApp",
      title: "ToDo App",
      // ...
      spec: [
        route("MainRoute", "/", page(MainPage, { authRequired: true })),
      ],
    });
    ```

  </TabItem>
</Tabs>

Note that `route` no longer references a page by name (`to: MainPage`); it takes the `page(...)` object directly.

The Wasp Spec has no `wasp` field. The `@wasp/wasp` version in `package.json` sets your project's Wasp version.

### Queries and actions

<Tabs sideBySide>
  <TabItem value="before" label="Wasp DSL">
    ```wasp title="main.wasp"
    query getTasks {
      fn: import { getTasks } from "@src/queries",
      entities: [Task]
    }

    action createTask {
      fn: import { createTask } from "@src/actions",
      entities: [Task]
    }
    ```

  </TabItem>
  <TabItem value="after" label="Wasp Spec">
    ```ts title="main.wasp.ts"
    import { action, app, query } from "@wasp/spec";
    import { getTasks } from "./src/queries" with { type: "ref" };
    import { createTask } from "./src/actions" with { type: "ref" };

    export default app({
      // ...
      spec: [
        query(getTasks, { entities: ["Task"] }),
        action(createTask, { entities: ["Task"] }),
      ],
    });
    ```

  </TabItem>
</Tabs>

### APIs: `httpRoute` becomes positional arguments

The DSL's `httpRoute: (GET, "/path")` becomes the first two arguments of `api`.

<Tabs sideBySide>
  <TabItem value="before" label="Wasp DSL">
    ```wasp title="main.wasp"
    apiNamespace bar {
      middlewareConfigFn: import { barNamespaceMiddlewareFn } from "@src/apis",
      path: "/bar"
    }

    api barBaz {
      fn: import { barBaz } from "@src/apis",
      auth: false,
      entities: [Task],
      httpRoute: (GET, "/bar/baz")
    }
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

The DSL's `perform: { fn, executorOptions }` is flattened: `fn` becomes the first argument and `executorOptions` becomes `performExecutorOptions`.

<Tabs sideBySide>
  <TabItem value="before" label="Wasp DSL">
    ```wasp title="main.wasp"
    job mySpecialJob {
      executor: PgBoss,
      perform: {
        fn: import { foo } from "@src/jobs/bar",
        executorOptions: { pgBoss: {=json { "retryLimit": 1 } json=} }
      },
      entities: [Task]
    }
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
  <TabItem value="before" label="Wasp DSL">
    ```wasp title="main.wasp"
    crud tasks {
      entity: Task,
      operations: {
        getAll: {},
        create: { overrideFn: import { createTask } from "@src/actions" }
      }
    }
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

These were top-level fields of the `app` declaration's dictionary in the DSL. In the Wasp Spec they are keys of the `app({ ... })` object.

<Tabs sideBySide>
  <TabItem value="before" label="Wasp DSL">
    ```wasp title="main.wasp"
    app todoApp {
      title: "ToDo App",
      auth: {
        userEntity: User,
        methods: { google: {} },
        onAuthFailedRedirectTo: "/login"
      },
      client: {
        rootComponent: import App from "@src/App"
      },
      emailSender: {
        provider: SMTP,
        defaultFrom: { email: "hi@example.com" }
      }
    }
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

When you reach [Migration from 0.23 to 0.24](/docs/0.24/migration-guide), convert your config with [the Wasp 0.24 version of this guide](/docs/0.24/guides/legacy/wasp-dsl#how-to-migrate), which has the steps for Wasp 0.24. Then continue with the migration guides up to the [latest one](../../migration-guide.md).

The [changes above](#changes) show the Wasp Spec of the latest Wasp version, so they show how your config looks once you finish.

See the full [Wasp Spec reference](../../features/spec.md#reference) for every option. Got stuck? Reach out on our [Discord](https://discord.gg/rzdnErX) and we'll help.
