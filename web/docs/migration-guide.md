---
sidebar_label: From 0.26 to 0.27
---

# Migration from 0.26 to 0.27

## What's new in 0.27?

### Wasp as a project dependency

Your project now depends on Wasp through the `@wasp/wasp` npm package, which brings the Wasp CLI and the Wasp Spec. The `@wasp/wasp` version in `package.json` sets the project's Wasp version, so the `wasp: { version }` field in `main.wasp.ts` is gone. `npm install` installs Wasp with the rest of your dependencies, so the `wasp install` command is gone too. The global `wasp` command keeps working. Inside a project, it hands each command over to the project's own Wasp.

### Node.js 26

Wasp now requires Node.js 26 or later, the [current LTS line](https://github.com/nodejs/release#release-schedule). It enables the [Temporal API](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal) by default and removes some long-deprecated APIs. Check out Node.js's [release notes](https://nodejs.org/en/blog/release/v26.0.0) for the full list of changes.

### No npm workspaces and no automatic installs

Wasp projects no longer use npm workspaces, and Wasp no longer runs `npm install` for you. Your `package.json` lists everything your app needs, including the packages of the Wasp features you use, like `lucia` for auth. You install them with `npm install`, like in any other project. Your `package-lock.json` no longer changes when Wasp regenerates its code.

### New npm organization

Wasp's npm packages moved from the `@wasp.sh` organization to `@wasp`. The Wasp CLI is now published as `@wasp/cli`, and the Wasp Spec is `@wasp/spec`.

### Prisma 6

Wasp now uses Prisma 6. Check out Prisma's [upgrade guide](https://www.prisma.io/docs/orm/more/upgrade-guides/upgrading-versions/upgrading-to-prisma-6) for the full list of changes.

### Testing with your own Vitest setup

Wasp no longer installs and runs [Vitest](https://vitest.dev/) for you. You set it up like in any other Vite app, so you choose the versions of Vitest and the testing libraries.

The helpers for rendering components and mocking queries and APIs moved from `wasp/client/test` to the `@wasp/test-utils` package, and work the same. Read more in the [testing docs](./advanced/testing.md).

### Smaller Docker images

The server's Docker image now contains only the files the server needs at runtime, instead of the whole `node_modules` folder. This makes the image about half the size.

Wasp finds those files by analyzing the server's imports. Some packages load their files in a non-standard way that this analysis can't follow, and you might need to list them in `server.traceDeps`.

The build steps are also ordered so that a code change reuses the installed dependencies, which makes rebuilding the image after a code change faster.

## How to migrate?

### 1. Upgrade Node.js to 26.13.1 or later {#upgrade-nodejs}

Wasp 0.27 requires Node.js >= 26.13.1 (previously >= 24.22.1). The new Wasp CLI refuses to run on older versions, so upgrade before continuing:

```shell
node -v
# If below 26.13.1, upgrade:
nvm install 26
```

If you deploy with the Dockerfile Wasp generates, your server image moves to `node:26.13.1-alpine3.23` when you rebuild it. If you use a custom Dockerfile, a CI workflow, or a platform that picks its own Node.js version, make sure it runs 26.13.1 or newer.

Wasp also requires `@types/node` to match the Node.js major version. Update it in your `package.json`:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```json title="package.json"
    {
      "devDependencies": {
        // highlight-next-line
        "@types/node": "^24.0.0"
      }
    }
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```json title="package.json"
    {
      "devDependencies": {
        // highlight-next-line
        "@types/node": "^26.0.0"
      }
    }
    ```
  </TabItem>
</Tabs>

Node.js 26 is a new major version with breaking changes. Check out Node.js's [release notes](https://nodejs.org/en/blog/release/v26.0.0) to see if any of them affect your app.

### 2. Switch to the new global Wasp CLI package

The global Wasp CLI is now published as `@wasp/cli` instead of `@wasp.sh/wasp-cli`. Uninstall the old package. It doesn't hand commands over to your project's Wasp, so `wasp start` would keep running Wasp 0.26 on your migrated project:

<PackageManagerCommand npm="npm uninstall -g @wasp.sh/wasp-cli" />

**If you don't want a global Wasp CLI, you can skip the rest of this step.** Your project's `@wasp/wasp` dependency brings the Wasp CLI, and you run it through npm (see [Running Wasp commands](./advanced/cli.md#running-wasp-commands)).

To keep typing `wasp <command>`, install the new package. If you skipped the uninstall above, npm fails here with an `EEXIST` error:

<PackageManagerCommand npm="npm i -g @wasp/cli@0.27" />

### 3. Depend on `@wasp/wasp` instead of `@wasp.sh/spec` {#depend-on-wasp}

Replace the `@wasp.sh/spec` devDependency with an exact `@wasp/wasp` dependency. Remove Wasp's two entries from `workspaces`, and add the `wasp` dependency in their place. The `wasp` dependency points at the code Wasp generates, and your `wasp/...` imports resolve to it. If `workspaces` has no other entries, remove the field.

Also add the npm scripts that new projects come with. They run your project's Wasp without a global install, and the docs' npm commands use them, for example `npm run dev` or `npm run wasp -- db migrate-dev --name init`. See [Running Wasp commands](./advanced/cli.md#running-wasp-commands):

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```json title="package.json"
    {
      // highlight-start
      "workspaces": [
        ".wasp/out/*",
        ".wasp/out/sdk/wasp"
      ],
      // highlight-end
      "devDependencies": {
        // highlight-next-line
        "@wasp.sh/spec": "file:.wasp/spec"
      }
    }
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```json title="package.json"
    {
      // highlight-start
      "scripts": {
        "wasp": "wasp",
        "dev": "wasp start",
        "build": "wasp build",
        "preview": "wasp build start"
      },
      "dependencies": {
        "@wasp/wasp": "0.27.0",
        "wasp": "file:.wasp/out/sdk/wasp"
      }
      // highlight-end
    }
    ```
  </TabItem>
</Tabs>

Don't run `npm install` yet. `@wasp/wasp` requires Prisma 6, so npm fails with a dependency conflict while `package.json` still lists Prisma 5. You'll install the dependencies in [step 10](#reinstall-dependencies).

### 4. Add the packages of the features you use {#feature-packages}

**If your app uses none of the features in the table below, you can skip this step.**

Wasp used to install the packages that only some features need. Now your `package.json` lists them. Add the packages of the features your app uses to `dependencies`. `@types/nodemailer` belongs in `devDependencies`:

| Feature | Packages |
| --- | --- |
| [Auth](./features/auth/overview.md) | `lucia` |
| [Social auth](./features/auth/social-auth/overview.md) | `lucia`, `arctic` |
| [Jobs](./features/jobs.md) | `pg-boss` |
| [WebSockets](./features/websockets.md) | `socket.io`, `socket.io-client`, `@socket.io/component-emitter` |
| [SMTP email sender](./features/email.md#smtp) | `nodemailer`, `@types/nodemailer` |
| [Mailgun email sender](./features/email.md#mailgun) | `mailgun.js` |
| [SendGrid email sender](./features/email.md#sendgrid) | `@sendgrid/mail` |
| [Resend email sender](./features/email.md#resend) | `resend` |

For example, an app with username auth and a job adds:

```json title="package.json"
{
  "dependencies": {
    // highlight-start
    "lucia": "^3.0.1",
    "pg-boss": "^8.4.2"
    // highlight-end
  }
}
```

You don't have to get the list right now. If a package is missing, Wasp stops with an error that names the package and the command that installs the version Wasp needs.

### 5. Move your overrides of Wasp's dependencies to npm `overrides`

**If your `package.json` has no `wasp.overriddenDeps` field, you can skip this step.**

Wasp no longer reads the `wasp.overriddenDeps` field. Override a package with npm's `overrides` field instead, and remove `wasp.overriddenDeps`.

The two fields take different versions. `wasp.overriddenDeps` lists the version Wasp requires, while `overrides` lists the version you want. So don't copy the value over. Set the override to the version from your `dependencies`, since npm fails with `EOVERRIDE` when the two don't match exactly:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```json title="package.json"
    {
      "dependencies": {
        "react": "18.2.0"
      },
      // highlight-start
      "wasp": {
        "overriddenDeps": {
          "react": "19.2.1"
        }
      }
      // highlight-end
    }
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```json title="package.json"
    {
      "dependencies": {
        "react": "18.2.0"
      },
      // highlight-start
      "overrides": {
        "react": "18.2.0"
      }
      // highlight-end
    }
    ```
  </TabItem>
</Tabs>

Read more in [Overriding Wasp's dependencies](./advanced/dependencies.md#overriding-wasps-dependencies-advanced).

### 6. Check your `.npmrc`

**If your project has no `.npmrc` file, you can skip this step.**

The packages that come with `@wasp/wasp` are its peer dependencies, so npm has to install peer dependencies. If your `.npmrc` sets `legacy-peer-deps=true` or `omit=peer`, remove that setting. With it, npm skips these packages and your app fails with missing modules.

If your `.npmrc` sets `min-release-age`, npm won't install packages published in the last 7 days, and that includes a new Wasp release. Exclude Wasp's packages, so you can upgrade on the day a release comes out:

```ini title=".npmrc"
min-release-age=7
# highlight-next-line
min-release-age-exclude[]=@wasp/*
```

### 7. Rename `@wasp.sh/spec` imports to `@wasp/spec`

Wasp now publishes the Wasp Spec as `@wasp/spec`, so update the imports in your `main.wasp.ts` and any other `*.wasp.ts` files:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```ts title="main.wasp.ts"
    // highlight-next-line
    import { app, page, route } from "@wasp.sh/spec";
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```ts title="main.wasp.ts"
    // highlight-next-line
    import { app, page, route } from "@wasp/spec";
    ```
  </TabItem>
</Tabs>

If your project has an `AGENTS.md` or `CLAUDE.md` from a Wasp starter, update or remove the lines that mention the old package, the Wasp version in `main.wasp.ts`, or `wasp install`, so coding agents don't follow outdated instructions:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```md title="AGENTS.md"
    - Pick the docs map matching the Wasp version in `main.wasp.ts`.
    - `main.wasp.ts` contains the Wasp version and the Wasp app specification.
    - `@wasp.sh/spec` is generated by Wasp, don't install it from npm.
    - Run `wasp install` before other Wasp commands in a fresh clone or worktree.
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```md title="AGENTS.md"
    - Pick the docs map matching the version of the `@wasp/wasp` dependency in `package.json`.
    - `main.wasp.ts` contains the Wasp app specification.
    - Run `npm install` before other Wasp commands in a fresh clone or worktree.
    ```
  </TabItem>
</Tabs>

### 8. Remove the Wasp version from `main.wasp.ts`

`package.json` now sets the Wasp version, so remove the `wasp` field:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```ts title="main.wasp.ts"
    export default app({
      name: "myApp",
      // highlight-next-line
      wasp: { version: "^0.26.0" },
      // ...
    });
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```ts title="main.wasp.ts"
    export default app({
      name: "myApp",
      // ...
    });
    ```
  </TabItem>
</Tabs>

### 9. Upgrade to Prisma 6

Update the Prisma packages in your `package.json`:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```json title="package.json"
    {
      "dependencies": {
        // highlight-next-line
        "@prisma/client": "5.19.1" // if already present
      },
      "devDependencies": {
        // highlight-next-line
        "prisma": "5.19.1"
      }
    }
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```json title="package.json"
    {
      "dependencies": {
        // highlight-next-line
        "@prisma/client": "6.19.3" // if already present
      },
      "devDependencies": {
        // highlight-next-line
        "prisma": "6.19.3"
      }
    }
    ```
  </TabItem>
</Tabs>

Prisma 6 also has a few breaking changes in Prisma Client. Check [Prisma's upgrade guide](https://www.prisma.io/docs/orm/more/upgrade-guides/upgrading-versions/upgrading-to-prisma-6) to see if any of them affect your app.

### 10. Reinstall your dependencies {#reinstall-dependencies}

:::caution Back up your SQLite development database
If your app uses SQLite in development, its database is `.wasp/out/db/dev.db`, so deleting `.wasp` deletes your development data too. Copy the file somewhere outside `.wasp` first if you want to keep it.
:::

Delete the old build output and `node_modules`, and install the dependencies:

```sh
rm -rf .wasp node_modules
npm install
```

Then run the migrations. Prisma 6 changed how it stores some things in the database, so <InlineWaspCommand command="wasp db migrate-dev" /> creates a new migration if your app needs one:

<WaspCommand command="wasp db migrate-dev" />

If Wasp reports a missing package from [step 4](#feature-packages) here, run the command Wasp prints and try again.

Finally, run `npm install` once more. Wasp has generated its code by now, so npm removes the old workspace entries from your `package-lock.json`. Commit the file.

### 11. Update your Vitest setup

The `wasp test` command is gone, and Wasp no longer installs testing libraries or configures Vitest for you. The testing helpers (`renderInContext` and `mockServer`) moved from `wasp/client/test` to the `@wasp/test-utils` package, which you now install yourself.

Follow the part that matches your app.

#### If you don't use `wasp test client`

Wasp no longer needs `vitest`, so you can remove it from your project:

1. If your `vite.config.ts` imports `defineConfig` from `vitest/config`, import it from `vite` instead, and remove the `test` section. Otherwise, Vite can't load your config after you remove `vitest`.

   <Tabs sideBySide>
     <TabItem value="before" label="Before">
       ```ts title="vite.config.ts"
       // highlight-next-line
       import { defineConfig } from "vitest/config";
       import { wasp } from "wasp/client/vite";

       export default defineConfig({
         plugins: [wasp()],
         // highlight-start
         test: {
           exclude: ["./e2e-tests/**"],
         },
         // highlight-end
       });
       ```
     </TabItem>
     <TabItem value="after" label="After">
       ```ts title="vite.config.ts"
       // highlight-next-line
       import { defineConfig } from "vite";
       import { wasp } from "wasp/client/vite";

       export default defineConfig({
         plugins: [wasp()],
       });
       ```
     </TabItem>
   </Tabs>

2. Remove `vitest` from your `devDependencies`:

   <Tabs sideBySide>
     <TabItem value="before" label="Before">
       ```json title="package.json"
       {
         "devDependencies": {
           // highlight-next-line
           "vitest": "^4.1.9",
           "vite": "^8.1.0"
         }
       }
       ```
     </TabItem>
     <TabItem value="after" label="After">
       ```json title="package.json"
       {
         "devDependencies": {
           "vite": "^8.1.0"
         }
       }
       ```
     </TabItem>
   </Tabs>

3. Remove the `vitest/globals` and `@testing-library/jest-dom` references from `src/vite-env.d.ts`, if you have them:

   <Tabs sideBySide>
     <TabItem value="before" label="Before">
       ```ts title="src/vite-env.d.ts"
       /// <reference types="vite/client" />
       // highlight-start
       /// <reference types="vitest/globals" />
       /// <reference types="@testing-library/jest-dom" />
       // highlight-end
       ```
     </TabItem>
     <TabItem value="after" label="After">
       ```ts title="src/vite-env.d.ts"
       /// <reference types="vite/client" />
       ```
     </TabItem>
   </Tabs>

#### If you use `wasp test client`

1. Install Vitest, the testing libraries your tests use, and Wasp's testing helpers:

   <PackageManagerCommand npm="npm install -D vitest jsdom @testing-library/react @testing-library/jest-dom msw @wasp/test-utils" />

   Wasp's testing helpers need Vitest 5 and MSW 3, so this also upgrades Vitest if your project is on an older version. If your tests use Vitest features that changed, check [Vitest's migration guide](https://vitest.dev/guide/migration/).

2. Set up Vitest in your `vite.config.ts` with Wasp's `waspTestConfig` helper. It sets the options Wasp used to set for you:

   <Tabs sideBySide>
     <TabItem value="before" label="Before">
       ```ts title="vite.config.ts"
       import { defineConfig } from "vite";
       import { wasp } from "wasp/client/vite";

       export default defineConfig({
         plugins: [wasp()],
       });
       ```
     </TabItem>
     <TabItem value="after" label="After">
       ```ts title="vite.config.ts"
       // highlight-next-line
       import { waspTestConfig } from "@wasp/test-utils/config";
       // highlight-next-line
       import { defineConfig } from "vitest/config";
       import { wasp } from "wasp/client/vite";

       export default defineConfig({
         plugins: [wasp()],
         // highlight-start
         test: waspTestConfig({
           setupFiles: ["./src/test/setup.ts"],
         }),
         // highlight-end
       });
       ```
     </TabItem>
   </Tabs>

   Wasp used to make `test`, `expect`, and the other test functions available globally. If your tests use them without importing them, pass `globals: true` to `waspTestConfig`:

   <Tabs sideBySide>
     <TabItem value="before" label="Before">
       ```ts title="vite.config.ts"
       test: waspTestConfig({
         setupFiles: ["./src/test/setup.ts"],
       }),
       ```
     </TabItem>
     <TabItem value="after" label="After">
       ```ts title="vite.config.ts"
       test: waspTestConfig({
         setupFiles: ["./src/test/setup.ts"],
         // highlight-next-line
         globals: true,
       }),
       ```
     </TabItem>
   </Tabs>

   Keep the `vitest/globals` types in `src/vite-env.d.ts`, so TypeScript knows about the globals when Wasp type-checks your tests:

   ```ts title="src/vite-env.d.ts"
   /// <reference types="vite/client" />
   // highlight-next-line
   /// <reference types="vitest/globals" />
   ```

   Or import them from `vitest` in each test file instead:

   <Tabs sideBySide>
     <TabItem value="before" label="Before">
       ```ts title="src/utils.test.ts"
       import { add } from "./utils";

       test("adds numbers", () => {
         expect(add(1, 2)).toBe(3);
       });
       ```
     </TabItem>
     <TabItem value="after" label="After">
       ```ts title="src/utils.test.ts"
       // highlight-next-line
       import { expect, test } from "vitest";
       import { add } from "./utils";

       test("adds numbers", () => {
         expect(add(1, 2)).toBe(3);
       });
       ```
     </TabItem>
   </Tabs>


3. Create the setup file you referenced in `setupFiles`. It adds the DOM matchers, like `toBeInTheDocument()`, and removes what a test rendered when the test ends:

   ```ts title="src/test/setup.ts"
   import "@testing-library/jest-dom/vitest";
   import { cleanup } from "@testing-library/react";
   import { afterEach } from "vitest";

   afterEach(() => {
     cleanup();
   });
   ```

   The `import "@testing-library/jest-dom/vitest"` line also adds the types for the DOM matchers, so you can remove the `@testing-library/jest-dom` reference from `src/vite-env.d.ts`:

   <Tabs sideBySide>
     <TabItem value="before" label="Before">
       ```ts title="src/vite-env.d.ts"
       /// <reference types="vite/client" />
       /// <reference types="vitest/globals" />
       // highlight-next-line
       /// <reference types="@testing-library/jest-dom" />
       ```
     </TabItem>
     <TabItem value="after" label="After">
       ```ts title="src/vite-env.d.ts"
       /// <reference types="vite/client" />
       /// <reference types="vitest/globals" />
       ```
     </TabItem>
   </Tabs>

4. Import the testing helpers from `@wasp/test-utils` instead of `wasp/client/test`:

   <Tabs sideBySide>
     <TabItem value="before" label="Before">
       ```ts title="src/TaskList.test.tsx"
       // highlight-next-line
       import { mockServer, renderInContext } from "wasp/client/test";
       ```
     </TabItem>
     <TabItem value="after" label="After">
       ```ts title="src/TaskList.test.tsx"
       // highlight-next-line
       import { mockServer, renderInContext } from "@wasp/test-utils";
       ```
     </TabItem>
   </Tabs>

5. Run your tests with Vitest instead of `wasp test client`. Add a `test` script to your `package.json`, or change it if it runs `wasp test client`:

   <Tabs sideBySide>
     <TabItem value="before" label="Before">
       ```json title="package.json"
       {
         "scripts": {
           // highlight-next-line
           "test": "wasp test client"
         }
       }
       ```
     </TabItem>
     <TabItem value="after" label="After">
       ```json title="package.json"
       {
         "scripts": {
           // highlight-next-line
           "test": "vitest"
         }
       }
       ```
     </TabItem>
   </Tabs>

   Your tests import the Wasp SDK, so generate it first with <InlineWaspCommand command="wasp compile" />, or keep <InlineWaspCommand command="wasp start" /> running in another terminal:

   <WaspCommand command={"wasp compile\nnpm test"} />

Read more in the [testing docs](./advanced/testing.md).

### 12. Check relative paths in your server code

The server now runs with your project's root directory as its working directory, instead of `.wasp/out/server`. In the Docker image, that's `/app` instead of `/app/.wasp/out/server`.

If your server code reads or writes files using relative paths, they now resolve from the project root. If you worked around the old behavior (e.g. with paths like `../../../src/data/countries.json`), remove the workaround. If you deploy with Docker and mount a volume at a path inside `/app/.wasp/out/server`, move it to the matching path inside `/app`.

### 13. Check the packages your server loads at runtime

**If you didn't add any npm packages to your app yourself, you can skip this step.** The packages Wasp uses work out of the box.

Otherwise, check if any of the packages your server uses serve or load files from their own folder, like `swagger-ui-express`.

The Docker image now contains only the files Wasp finds by following the server's imports. A package that loads files in a non-standard way misses them in the image. A common case is a package that serves static assets from its own folder (e.g. `swagger-ui-express`), which would fail in production.

List such packages in [`server.traceDeps`](./deployment/methods/overview#missing-files-in-the-docker-image) to copy all of their files:

```ts title="main.wasp.ts"
export default app({
  // ...
  server: {
    traceDeps: ["swagger-ui-dist*"],
  },
})
```

### 14. Install the system dependencies your app needs

**If your app doesn't run Python on the server, you can skip this step.**

The generated Dockerfile no longer installs `python3` in the final image, because the Wasp server doesn't use it. If your app runs Python, install it in a [custom Dockerfile](./deployment/methods/overview#customizing-the-dockerfile):

```dockerfile title="Dockerfile"
RUN apk add --no-cache python3
```

### 15. Update your custom Dockerfile

**If you don't have a [`Dockerfile`](./deployment/methods/overview#customizing-the-dockerfile) in your project's dir, you can skip this step.**

The generated Dockerfile changed. Run <InlineWaspCommand command="wasp dockerfile" /> to print it with your custom Dockerfile appended at the bottom, and check that your steps still apply to the new stages.

The final stage (`server-production`) no longer contains the `/app/node_modules` folder. It only has the server's bundle and the packages the bundle uses, in `/app/.wasp/out/server`. If your steps use other packages (for example, through `npx`), copy the full `node_modules` from the `server-builder` stage:

```dockerfile title="Dockerfile"
COPY --from=server-builder /app/node_modules /app/node_modules
```

### 16. Import `HttpError` from `wasp/errors`

The server and the client now use the same `HttpError` class, so it moved from `wasp/server` to `wasp/errors`. Update your imports:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```ts title="src/queries.ts"
    import { HttpError } from "wasp/server"
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```ts title="src/queries.ts"
    // highlight-next-line
    import { HttpError } from "wasp/errors"
    ```
  </TabItem>
</Tabs>

### 17. Update how you read errors on the client

On the client, failed operations and auth actions now throw an `HttpError`, the same class you throw on the server. It replaces `WaspHttpError` from `wasp/client/api`, and it differs in two ways:

- Its `data` is the `data` you passed on the server. Before, `data` held the whole response body, so you had to read `error.data.data`.
- It is part of the documented public API, so you can rely on checking for it with `instanceof`.

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```ts title="src/SignupPage.tsx"
    try {
      await signup({ username, password })
    } catch (error: any) {
      if (error.statusCode === 422) {
        setErrorMessage(error.data.data.message)
      }
    }
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```ts title="src/SignupPage.tsx"
    // highlight-next-line
    import { HttpError } from "wasp/errors"

    try {
      await signup({ username, password })
    } catch (error: unknown) {
      // highlight-next-line
      if (error instanceof HttpError && error.status === 422) {
        // highlight-next-line
        setErrorMessage(String(error.data?.message))
      }
    }
    ```
  </TabItem>
</Tabs>

### 18. Pass `HttpError` data in the options

`HttpError`'s third argument is now an options object, and the data is one of its options, next to `headers` and `cause`:

<Tabs sideBySide>
  <TabItem value="before" label="Before">
    ```ts title="src/queries.ts"
    throw new HttpError(403, "Not allowed", { reason: "NOT_OWNER" })
    ```
  </TabItem>
  <TabItem value="after" label="After">
    ```ts title="src/queries.ts"
    throw new HttpError(403, "Not allowed", {
      // highlight-next-line
      data: { reason: "NOT_OWNER" },
    })
    ```
  </TabItem>
</Tabs>

Its status code is also available as `status` now. `statusCode` still works, but it is deprecated.

### 19. Enjoy your updated Wasp app

That's it!
