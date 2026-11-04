---
title: Migrating from the Wasp DSL to the Wasp Spec
sidebar_label: "Wasp DSL"
comments: true
---

import LastCheckedWithVersionsNotice from "@site/src/components/LastCheckedWithVersionsNotice";

# Migrating from the Wasp DSL

<LastCheckedWithVersionsNotice versions={{ Wasp: "0.27" }} />

Wasp used to have its own configuration language, the **Wasp DSL**, which you wrote in a `main.wasp` file. Wasp 0.24 retired the Wasp DSL in favor of the [Wasp Spec](../../features/spec.md), a `main.wasp.ts` file written in TypeScript.

If your project still has a `main.wasp` file, it runs Wasp 0.23 or older. This guide converts it straight to the Wasp Spec setup of Wasp 0.27. [How to migrate](#how-to-migrate) also lists the migration guides for the other changes between your version and 0.27.

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

These steps convert a Wasp DSL project to the Wasp Spec setup of Wasp 0.27. The Wasp versions between yours and 0.27 changed other parts of a project too, so step 9 sends you through their migration guides.

Wasp validates the Wasp Spec support files during migration, including the required `package.json` entries, `tsconfig.wasp.json` options, and `tsconfig.src.json` exclusions.

1. Rename `tsconfig.json` to `tsconfig.src.json`. Keep its compiler options and add the entries Wasp needs:

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

3. Create a `tsconfig.wasp.json` for the Wasp Spec files:

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

4. Add Wasp to your `package.json`:

   ```json title="package.json"
   {
     // ...
     // highlight-start
     "scripts": {
       "wasp": "wasp"
     },
     // highlight-end
     "dependencies": {
       // ...
       // highlight-next-line
       "@wasp/wasp": "0.27.0"
     },
     "devDependencies": {
       // ...
       // highlight-next-line
       "@types/node": "^24.0.0"
     }
   }
   ```

   Keep your existing dependencies and add these entries. `@wasp/wasp` sets your project's Wasp version and brings the Wasp CLI and the Wasp Spec. The Wasp Spec runs in Node.js, so it needs `@types/node`. The `wasp` script is optional. It runs your project's Wasp through npm, for example `npm run wasp -- db migrate-dev`. Read more in [Running Wasp commands](../../advanced/cli.md#running-wasp-commands).

5. If your `.npmrc` sets `min-release-age`, exclude Wasp's packages from it. Otherwise npm won't install a new Wasp release during its first 7 days:

   ```ini title=".npmrc"
   min-release-age=7
   # highlight-next-line
   min-release-age-exclude[]=@wasp/*
   ```

6. Rename `main.wasp` to `main.wasp.old`.

7. Create a `main.wasp.ts` file with the following content:

   ```ts title="main.wasp.ts"
   import { app } from "@wasp/spec";

   export default app({
     name: "myApp",
     title: "My app",
     head: ["<link rel='icon' href='/favicon.ico' />"],
     spec: [
       // ...
     ],
   });
   ```

   :::note
   While previously we accepted any `*.wasp` file name, with the Wasp Spec the entry file must be named `main.wasp.ts`. You can still split the rest of your config across other `*.wasp.ts` files.
   :::

8. Rewrite your config:

   You can use the mapping above. Top-level concerns like `auth`, `server`, `client`, `db`, `emailSender`, and `webSocket` become keys of the `app({ ... })` object. Pages, routes, queries, actions, APIs, jobs, and CRUDs go into the `spec` property. Leave out the `wasp: { version }` field, since step 4 set the Wasp version in `package.json`.

9. Make the other changes from the migration guides between your Wasp version and 0.27. For a project on Wasp 0.23, follow these in order:

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

10. Install the dependencies, run the database migrations, and start your app:

    ```sh
    wasp clean
    npm install
    wasp db migrate-dev
    wasp start
    ```

    `@wasp/wasp` needs the dependency versions from the migration guides, like Prisma 6 and TypeScript 6. Until your `package.json` lists them, `npm install` fails with a dependency conflict. If everything is correct, your app behaves exactly as before.

11. Delete `main.wasp.old` once you're sure the new config works.

See the full [Wasp Spec reference](../../features/spec.md#reference) for every option. Got stuck? Reach out on our [Discord](https://discord.gg/rzdnErX) and we'll help.
