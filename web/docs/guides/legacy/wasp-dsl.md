---
sidebar_label: "Wasp DSL"
comments: true
---

import LastCheckedWithVersionsNotice from "@site/src/components/LastCheckedWithVersionsNotice";

# Migrating from the Wasp DSL

<LastCheckedWithVersionsNotice versions={{ Wasp: "0.24" }} />

Wasp used to have its own configuration language, the **Wasp DSL**, which you wrote in a `main.wasp` file. Starting with Wasp 0.24, the Wasp DSL is now retired in favor of the [Wasp Spec](../../general/spec.md): a `main.wasp.ts` file written in TypeScript.

:::tip Upgrading from Wasp 0.23 to 0.24?
The conversion below is mechanical, so you can let an LLM do the heavy lifting instead. The [migration guide](/docs/0.24/migration-guide#use-an-agent-to-do-it-for-you) has a copyable prompt bundling this guide, the Wasp Spec docs, and the shared migration steps. Once your config is converted, return to the [migration guide](/docs/0.24/migration-guide) for the remaining shared steps.
:::

## New features

### Just TypeScript

The Wasp DSL was a custom language, so it needed its own IDE extension for highlighting and autocompletion, and you couldn't use the JavaScript ecosystem inside it. The Wasp Spec is just TypeScript, so:

- No special IDE extension is needed. You get type checking, autocompletion, and go-to-definition from your editor's regular TypeScript support.
- You can `import` and use npm packages, environment variables, and your own helpers while building the config.
- You can use normal language features (variables, functions, loops, conditionals) to remove repetition from your config.

### Multiple files

The Wasp DSL kept your entire configuration in a single `main.wasp`. The Wasp Spec lets you split it across multiple `*.wasp.ts` files and import specifications between them, so you can keep large apps organized (for example, a separate `auth.wasp.ts` or `payments.wasp.ts` next to the feature it configures).

See the [Wasp Spec documentation](../../general/spec.md#splitting-your-spec-into-multiple-files) for details.

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

### App, routes, and pages

In the DSL, a `route` points to a `page` by name. In the Wasp Spec, `route` takes the `page` object directly.

<Tabs sideBySide>
  <TabItem value="before" label="Wasp DSL">
    ```wasp title="main.wasp"
    app todoApp {
      title: "ToDo App",
      wasp: { version: "^0.24.0" }
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
    import { app, page, route } from "@wasp.sh/spec";
    import { MainPage } from "./src/MainPage" with { type: "ref" };

    export default app({
      name: "todoApp",
      title: "ToDo App",
      wasp: { version: "^0.24.0" },
      spec: [
        route("MainRoute", "/", page(MainPage, { authRequired: true })),
      ],
    });
    ```

  </TabItem>
</Tabs>

Note that `route` no longer references a page by name (`to: MainPage`); it takes the `page(...)` object directly.

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
    import { action, app, query } from "@wasp.sh/spec";
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
    import { api, apiNamespace, app } from "@wasp.sh/spec";
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
    import { app, job } from "@wasp.sh/spec";
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
    import { app, crud } from "@wasp.sh/spec";
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
      wasp: { version: "^0.24.0" },
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
    import { app } from "@wasp.sh/spec";
    import App from "./src/App" with { type: "ref" };

    export default app({
      name: "todoApp",
      title: "ToDo App",
      wasp: { version: "^0.24.0" },
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

These steps convert a Wasp DSL config to the Wasp Spec. Before running `wasp install` below, make sure your app's Wasp version is `^0.24.0`.

After finishing this guide, return to the [migration guide](/docs/0.24/migration-guide) if you still need to complete the shared Wasp 0.24 migration steps.

Wasp validates the Wasp Spec support files during migration, including the required `package.json` entries, `tsconfig.wasp.json` options, and `tsconfig.src.json` exclusions.

1. Rename `tsconfig.json` to `tsconfig.src.json` and make it exclude Wasp Spec files:

   ```json title="tsconfig.src.json"
   {
     // ...
     "include": ["src"],
     "exclude": ["**/*.wasp.ts", "**/*.wasp.tsx"]
   }
   ```

2. Create a new `tsconfig.json` that references the other two configs:

   ```json title="tsconfig.json"
   {
     "files": [],
     "references": [
       { "path": "./tsconfig.src.json" },
       { "path": "./tsconfig.wasp.json" }
     ]
   }
   ```

3. Create a `tsconfig.wasp.json` with the required compiler options and the Wasp Spec includes:

   ```json title="tsconfig.wasp.json"
   {
     "compilerOptions": {
       "target": "ES2025",
       "module": "esnext",
       "moduleResolution": "bundler",
       "jsx": "react-jsx",
       "strict": true,
       "isolatedModules": true,
       "moduleDetection": "force",
       "skipLibCheck": true,
       "allowJs": true,
       "noEmit": true,
       "lib": ["ES2025"],
       "types": ["node"]
     },
     "include": ["**/*.wasp.ts", "**/*.wasp.tsx", ".wasp/out/types/spec"]
   }
   ```

4. Add the required `devDependencies` to your `package.json`:

   ```json title="package.json"
   {
     // ...
     "devDependencies": {
       // ...
       // highlight-next-line
       "@types/node": "^24.0.0",
       // highlight-next-line
       "@wasp.sh/spec": "file:.wasp/spec"
     }
   }
   ```

   Keep your existing dependencies, and add these entries. `@types/node` is required because the Wasp Spec runs in a Node.js environment, and `@wasp.sh/spec` provides the local Wasp Spec API package.

5. Run `wasp install`.

6. Rename `main.wasp` to `main.wasp.old`.

7. Create a `main.wasp.tsx` file with the following content:

   ```tsx title="main.wasp.tsx"
   import { app } from "@wasp.sh/spec";

   export default app({
     name: "myApp",
     title: "My app",
     wasp: { version: "^0.24.0" },
     head: [<link rel="icon" href="/favicon.ico" />],
     spec: [
       // ...
     ],
   });
   ```

   :::note
   While previously we accepted any `*.wasp` file name, with the Wasp Spec the entry file must be named `main.wasp.tsx` (or `main.wasp.ts` if it doesn't use JSX, e.g. when it has no `head`). You can still split the rest of your config across other `*.wasp.ts(x)` files.
   :::

8. Rewrite your config:

   You can use the mapping above. Top-level concerns (e.g. `auth`, `server`, `client`, `db`, `emailSender`, `webSocket`) become keys of the `app({ ... })` object; pages, routes, queries, actions, APIs, jobs, and CRUDs go into the `spec` property.

9. Run your app with `wasp start`. If everything is correct, your app should behave exactly as before.

:::note
At some points, when the Spec needs to be regenerated, Wasp will tell you to run `wasp install` before being able to start the app. Usually, this might happen when upgrading Wasp versions, running `wasp clean`, or removing the `node_modules` folder.
:::

10. Delete `main.wasp.old` once you're sure the new config works.

See the full [Wasp Spec reference](../../general/spec.md#reference) for every option. Got stuck? Reach out on our [Discord](https://discord.gg/rzdnErX) and we'll help.
